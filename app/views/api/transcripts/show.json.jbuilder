json.array! @captions do |caption|
    json.name caption[:name]
    json.set! "captions" do
        json.array! caption[:captions] do |line|
                json.time line[:time].to_f
                json.text line[:text]
        end
    end
end