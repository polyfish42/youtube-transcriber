json.transcript do 
    @lines.each do |line|
        json.set! line[:id] do
            json.extract! line, :time, :text
        end
    end
end