#!/bin/env ruby
require 'pry-byebug'

Node = Struct.new(:children, :metas) do
  def depth_first(&blk)
    children.each { |ch| ch.depth_first(&blk) }
    blk.call(self)
  end

  def value
    if children.any?
      metas
        .map { |i| children[i - 1] }
        .compact
        .inject(0) { |sum, node| sum + node.value }
    else
      metas.inject(:+)
    end
  end
end

$nums = IO.read(ARGV[0]).split(/\s+/).map(&:to_i)

class Part1And2
  def call_1
    root, idx = build_node($nums, 0)
    sum = 0
    root.depth_first { |node| sum += node.metas.inject(:+) }
  end

  def call_2
    root, idx = build_node($nums, 0)
    root.value
  end

  private

  def build_node(nums, i)
    num_children = nums[i]
    num_metas = nums[i += 1]
    children = []
    metas = []

    num_children.times do |n|
      ch, i = build_node(nums, i += 1)
      children << ch
    end

    num_metas.times { metas << nums[i += 1] }

    [Node.new(children, metas), i]
  end
end

puts format("1: %s\n2: %p",
            Part1And2.new.call_1,
            Part1And2.new.call_2)
