require 'open-uri'
class Api::TranscriptsController < ApplicationController
    def show
        video_uri = params[:uri]
        res = Net::HTTP.get_response(URI(video_uri))
        
        if res.body.include?("captionTracks")
            xml_uri = caption_url(res)
            doc = Nokogiri::HTML(open(xml_uri))

            @lines = lines(doc)

            render "api/transcripts/show"
        else
            render json: "This video has no captions", status: 500
        end
    end

    private

    def lines(doc)
        lines = []
        uid = 0

        doc.xpath("//transcript").children.each do |text|
            line = {}
            line[:time] = text.attributes["start"].value
            line[:text] = remove_tags(text.children.inner_text)
            line[:id] = uid

            lines << line
            uid += 1
        end
        lines
    end

    def caption_url(res)
        s = StringScanner.new(res.body)
        s.skip_until(/captionTracks\\":\[\{\\"baseUrl\\":\\"/)
        uri = s.scan_until(/,/)
        uri.gsub!(/\\\\u0026/, "&").gsub!(/\\/, "").chomp!("\",")
    end

    def remove_tags(str)
        result = ""
        in_tag = false

        str.chars.each do |s|
            if s == ">"
                in_tag = false
            elsif in_tag
                next
            elsif s == "<"
                in_tag = true
            else
                result += s
            end
        end
        result
    end
end