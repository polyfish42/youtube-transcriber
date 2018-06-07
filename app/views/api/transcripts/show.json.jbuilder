json.array! @lines do |line|
    json.time line[:time].to_f
    json.text line[:text]
end