csv = []
File.open("simulation_results.txt", "r") do |f|
  f.each_line do |line|
    # puts line
    csv << line.gsub(/(:|{|\[|,|\]|[a-zA-Z])/, "").gsub(/}/, ",")
    puts csv
    end
end

File.open("simulation_results.csv", "w") do |f|
  f.write(csv.join(""))
end