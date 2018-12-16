csv = []
File.open("simulation_results.txt", "r") do |f|
  f.each_line do |line|
    csv << line.gsub(/(:|{|\[|,|\]|[a-zA-Z])/, "").gsub(/}/, ",")
    end
end

File.open("simulation_results.csv", "w") do |f|
  f.write(csv.join(""))
end