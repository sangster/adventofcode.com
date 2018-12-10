#!/bin/env ruby
$lines = IO.read(ARGV[0]).lines.map(&:strip)

Light = Struct.new(:x, :y, :dx, :dy)

class Part1And2

  def call_1
    calc { |_, str| puts str; return "see above" }
  end

  def call_2
    calc { |t, _| return t }
  end

  private

  def calc
    lights = $lines.map(&method(:parse))
    t = 0
    width = 99999
    old_str = nil

    sort_lights!(lights)
    t = inc_until_bounded(t, lights, lights.size)

    loop do
      str = draw(lights)

      twidth = str.lines.first.size
      if twidth >= width
        yield [t, old_str]
        return
      end
      width = twidth
      old_str = str

      inc(lights)
      sort_lights!(lights)
      t += 1
    end
  end

  def parse(str)
    m = /<\s*([+-]?\d+),\s+([+-]?\d+)>.+<\s*([+-]?\d+),\s*([+-]?\d+)>/.match(str)
    Light.new(m[1].to_i - 1, m[2].to_i - 1, m[3].to_i, m[4].to_i)
  end

  def sort_lights!(lights)
    lights.sort_by! { |light| [light.y, light.x] }
  end

  def draw(lights)
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

  def inc(lights)
    lights.each do |light|
      light.x += light.dx
      light.y += light.dy
    end
  end

  def inc_until_bounded(t, lights, dist)
    until bounded?(lights, dist)
      inc(lights)
      t += 1
    end
    t
  end

  def bounded?(lights, dist)
    lights.none? { |light| light.x.abs > dist || light.y.abs > dist }
  end
end

puts format("1: %p\n2: %p",
            Part1And2.new.call_1,
            Part1And2.new.call_2)
