#!/bin/env ruby
require 'pry-byebug'

NUM_WORKERS = 5
TIME_OVERHEAD = 60
$lines = IO.read(ARGV[0]).lines.map(&:strip)

class Part1And2
  def call_1
    map  = prereqs_map($lines.map(&method(:parse)))
    ordered = []
    ready = no_prereqs(map) - ordered

    while ready.any?
      step = ready.shift
      ordered << step
      remove_prereq(map, step)
      ready = (ready + no_prereqs(map) - ordered).sort.uniq
    end

    ordered.join
  end

  def call_2
    map  = prereqs_map($lines.map(&method(:parse)))
    seen = []
    ready = no_prereqs(map)
    jobs = []
    time = -1

    while ready.any? || jobs.any?
      jobs.each { |j| j[1] -= 1 }
      finished, jobs = jobs.partition { |j| j[1] == 0 }
      finished.each do |fin|
        remove_prereq(map, fin[0])
        ready = (ready + no_prereqs(map) - seen).sort.uniq
      end

      while jobs.size < NUM_WORKERS && ready.any?
        step = ready.shift
        seen << step
        jobs << [step, TIME_OVERHEAD + step.ord - 0x40]
      end

      time += 1
    end

    time
  end

  private

  def parse(str)
    [str[5], str[36]]
  end

  def prereqs_map(deps)
    Hash.new { |h, k| h[k] = [] }.tap do |map|
      deps.each { |(par, ch)| map[par]; map[ch] << par }
      map.values.each(&:sort!)
    end
  end

  def no_prereqs(map)
    map.keys.select { |k| map[k].empty? }.sort
  end

  def remove_prereq(map, step)
    map.values.each { |prereqs| prereqs.delete(step) }
  end
end

printf("1: %s\n2: %p\n",
            Part1And2.new.call_1,
            Part1And2.new.call_2)
