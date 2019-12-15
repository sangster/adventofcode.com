#!/bin/env ruby
$lines = IO.read(ARGV[0]).lines.map(&:strip)

Light = Struct.new(:x, :y, :dx, :dy)

class Part1And2

  def call_1
    lights = $lines.map(&method(:parse))

    pattern, _ = resolve_pattern(lights)

    puts pattern
    'see above'
  end

  def call_2
    lights = $lines.map(&method(:parse))
    _, time = resolve_pattern(lights)
    time
  end

  private

  def parse(str)
    m = /<\s*([+-]?\d+),\s+([+-]?\d+)>.+<\s*([+-]?\d+),\s*([+-]?\d+)>/.match(str)
    Light.new(m[1].to_i - 1, m[2].to_i - 1, m[3].to_i, m[4].to_i)
  end

  def resolve_pattern(lights)
    candidate = nil
    candidate_width = lights.size
    t = inc_until_bounded!(0, lights, candidate_width)

    loop do
      pattern = render_pattern(lights)
      width = pattern.lines.first.size

      return [candidate, t] if width >= candidate_width

      candidate = pattern
      candidate_width = width

      inc!(lights)
      sort_lights!(lights)
      t += 1
      # print pattern; sleep 0.075 # watch it happen
    end
  end

  def sort_lights!(lights)
    lights.sort_by! { |light| [light.y, light.x] }
  end

  def render_pattern(lights)
    coords = lights.map { |l| [l.x, l.y] }.uniq
    origin_x = coords.map(&:first).min
    origin_y = coords.map(&:last).min
    x = y = 0

    str = ''
    coords.each do |coord|
      lx = coord.first - origin_x
      ly = coord.last - origin_y
      while y < ly
        str += "\n"
        y += 1
        x = 0
      end
      while x < lx - 1
        str += ' '
        x += 1
      end
      str += '#'
      x += 1
    end

    str += "\n\n"
    str
  end

  def inc!(lights)
    lights.each do |light|
      light.x += light.dx
      light.y += light.dy
    end
  end

  # Given a number of lights, we can assume the pattern fits inside some bounds
  def inc_until_bounded!(t, lights, size)
    sort_lights!(lights)
    until bounded?(lights, size)
      inc!(lights)
      t += 1
    end
    t
  end

  def bounded?(lights, size)
    lights.none? { |light| light.x.abs > size || light.y.abs > size }
  end
end

printf("1: %p\n2: %p\n",
            Part1And2.new.call_1,
            Part1And2.new.call_2)
