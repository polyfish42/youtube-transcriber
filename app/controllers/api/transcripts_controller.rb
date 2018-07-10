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
        doc.xpath("//transcript").children.reduce([]) do |lines, text|
            line = {
                time: text.attributes["start"].value,
                text: remove_tags(text.children.inner_text)
            }
            lines << line
        end
    end

    def caption_url(res)
        s = StringScanner.new(res.body)
        s.skip_until(/captionTracks\\":\[\{\\"baseUrl\\":\\"/)
        url = s.scan_until(/,/)
        url.gsub(/\\\\u0026/, "&").gsub(/\\/, "").chomp("\",")
    end

    def remove_tags(str)
        tag_regexp = /<[^<|^>]+>/
        str.gsub(tag_regexp, "")
    end
end