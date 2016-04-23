#!/usr/bin/env perl
use strict;
use warnings;
use Test::More tests => 5;

{
	our $edges = [
		[qw/a f 9/],
		[qw/a e 4/],
		[qw/a g 10/],
		[qw/f e 7/],
		[qw/f c 2/],
		[qw/c e 1/],
		[qw/e d 6/],
		[qw/e b 2/],
		[qw/c d 40/],
		[qw/b d 7/],
		[qw/b h 3/],
		[qw/d h 2/],
	];
	diag('basic graph testing');
	my $path     = shortest_path($edges, 'a', 'h');
	my $expected = [qw/a e b h/];
	is_deeply(
		$path,
		$expected,
		'got expected results for a to h: ' . join(', ', @$path)
	);
	my $next_path     = shortest_path($edges, 'c', 'd');
	my $next_expected = [qw/c e d/];
	is_deeply(
		$next_path,
		$next_expected,
		'got shortest path to c to d: ' . join(', ', @$next_path)
	);
	diag('graph testing with doubled edges, loops, and higher cost than $inf');
	push(@$edges, [qw/b b 300/]);
	push(@$edges, [qw/c f 5000/]);
	push(@$edges, [qw/a c 2000000/]);
	$path     = shortest_path($edges, 'a', 'c');
	$expected = [qw/a e c/];
	is_deeply(
		$path,
		$expected,
		'got expected results with an odd data-set: ' . join(', ', @$path)
	);
	diag('graph tests when there are two equally shortest paths');
	push(@$edges, [qw/a i 4/]);
	push(@$edges, [qw/i j 2/]);
	push(@$edges, [qw/j h 3/]);
	my $path1_count  = 0;
	my $path2_count  = 0;
	my $safety_count = 0;
	while (!$path1_count || !$path2_count) {
		$safety_count++;
		last if ($safety_count > 100);
		$path = shortest_path($edges, 'a', 'h');
		$path1_count++ if (join('', @$path) eq 'aebh');
		$path2_count++ if (join('', @$path) eq 'aijh');
	}
	diag("tried [$safety_count] times to get each path at least once");
	ok($path1_count > 0, 'got shortest path a, e, b, h at least once');
	ok($path2_count > 0, 'got shortest path a, i, j, h at least once');
}

sub shortest_path {
	my ($graph, $source, $dest) = @_;
	# we can't adequately represent infinite in Perl, but we only need
	# it for comparison, so we'll start off with something high and then
	# if we find an edge that costs more we'll up it.
	my $inf = 10000;
	# we'll use $vertices as a hash of keys which will
	# contain every vertex. Neighours will be a hash of vertices
	# as keys, where subkeys are other vertices, and the values
	# are the cost between the two.
	my $vertices   = {};
	my $neighbours = {};
	# the edges can give us a list of all the vertices
	# and a hash of all the neighbours for each vertex
	# with the associated "cost" between them.
	foreach my $edge (@$graph) {
		# v1 and v1 are the two vertices relevant in this edge
		my $v1   = $edge->[0];
		my $v2   = $edge->[1];
		my $cost = $edge->[2];
		# if any cost is higher than what we've called INF, we need
		# to increase our INF for later comparions
		$inf = $cost + 1 if ($cost > $inf);
		# throw away any edges that loop back to the current
		# vertex. If any vertex has an edge that loops directly
		# back to itself, it's useless to us.
		next if ($v1 eq $v2);
		# drop vertices as keys in a hash to put into an array later.
		$vertices->{$v1}++;
		$vertices->{$v2}++;
		# some pairs of vertices could have multiple edges between them.
		# only choose the shorter of the two.
		if (my $current_cost = $neighbours->{$v1}{$v2}) {
			unless ($current_cost < $cost) {
				$neighbours->{$v1}{$v2} = $cost;
			}
		}
		else {
			$neighbours->{$v1}{$v2} = $cost;
		}
		if (my $current_cost = $neighbours->{$v2}{$v1}) {
			unless ($current_cost < $cost) {
				$neighbours->{$v2}{$v1} = $cost;
			}
		}
		else {
			$neighbours->{$v2}{$v1} = $cost;
		}
	}
	# now $vertices contains keys of all the vertices in the graph,
	# so we'll just make an array of them
	my @vertices = keys(%$vertices);
	# distance represents the distance from the original source
	# vertices to any other vertices. Previous represents the last
	# cost known to get to that vertices.
	my $distance;
	my $previous;
	foreach my $vertex (@vertices) {
		$distance->{$vertex} = $inf;
		$previous->{$vertex} = undef;
	}
	# we know that the cost to ourself is 0
	$distance->{$source} = 0;
	my @iter = @vertices;
	while (scalar(@iter) > 0) {
		my $min = $inf;
		my $current_vertex;
		# go through each vertex and if we have a distance
		# determined to that vertex (from source), make that
		# the minimum cost.
		foreach my $vertex (@iter) {
			if ($distance->{$vertex} < $min) {
				$min = $distance->{$vertex};
				# store a copy of the vertex
				$current_vertex = $vertex;
			}
		}
		# remove the current vertex from the list we are
		# working on.
		@iter = grep {$_ ne $current_vertex} @iter;
		# if we still don't have a real distance to $u figured out,
		# or if $u is our target, we're done.
		if ($distance->{$current_vertex} == $inf || $current_vertex eq $dest) {
			last;
		}
		foreach my $neighbour (keys(%{$neighbours->{$current_vertex}})) {
			# get all the neighbours of of the current
			# vertex so that we can update our distances
			my $cost = $neighbours->{$current_vertex}{$neighbour};
			# $potential_distance is the distance we might say it is
			# from source to destination if we haven't already found
			# a better distance.
			my $potential_distance = $distance->{$current_vertex} + $cost;
			if ($potential_distance < $distance->{$neighbour}) {
				# if there's not already a known distance, or
				# our potential distance is less than the known
				# distance, that means the cost to use and to
				# that neighbour is "cheaper" through us. So we
				# update that distance and say that the previous
				# vertex visited before reaching the neighbour
				# is our $current_vertex
				$distance->{$neighbour} = $potential_distance;
				$previous->{$neighbour} = $current_vertex;
			}
		}
	}
	my @path;
	my $iter = $dest;
	# At this point $previous is a hash of the vertex that was traveled from
	# prior to the current vertex that was the shortest path to that vertex.
	# we use $iter as an iterator backwards through that path to make a list
	# of the vertices that must be traveled to achieve shortest path from
	# the source to the destination.
	while ($previous->{$iter}) {
		unshift(@path, $iter);
		$iter = $previous->{$iter};
	}
	return [($iter, @path)];
}

1;

__END__
