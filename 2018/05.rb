#!/bin/env ruby

$units = IO.read(ARGV[0]).strip.chars

class Part1And2
  def initialize
  end

  def call_remaining_count
    resolve($units.dup).size
  end

  def call_stripped
    unique_units($units).map { |ch| resolve(without_unit($units, ch)).size }.min
  end

  private

  def resolve(list)
    len = list.size
    i = 0

    while i < len - 2
      if (list[i].ord - list[i + 1].ord).abs == 0x20
        list.delete_at(i)
        list.delete_at(i)

        len -= 2
        i -= 2
      end

      i += 1
    end

    list
  end

  def without_unit(list, unit)
    list.dup.delete_if { |c| c.downcase == unit }
  end

  def unique_units(units)
    units.map(&:downcase).uniq
  end
end

puts format("1: %s\n2: %p",
            Part1And2.new.call_remaining_count,
            Part1And2.new.call_stripped)
