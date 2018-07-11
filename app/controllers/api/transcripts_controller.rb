require 'open-uri'
class Api::TranscriptsController < ApplicationController
    def show
        video_uri = params[:uri]
        res = Net::HTTP.get_response(URI(video_uri))

        if res.body.include?("captionTracks")
            @captions = parseCaptions(res)
            p @captions
            render "api/transcripts/show"
        else
            render json: "This video has no captions", status: 500
        end
    end

    private

    def unescape(str)
        eval %Q{"#{str}"}
    end

    def captions(res)
        s = StringScanner.new(res.body)
        s.skip_until(/captionTracks\\":/)
        captionsStr = s.scan_until(/\]/)
        JSON.parse(unescape(captionsStr))
    end

    def parseCaptions(res)
        captions(res).reduce([]) do |captions, caption|
            captions << {
                name: caption["name"]["simpleText"],
                captions: fetchCaptions(caption["baseUrl"])
            }
        end
    end

    def format_caption_text(str)
        tag_regexp = /<[^<|^>]+>/
        str.gsub!(tag_regexp, "")
        str.gsub!(/\n/, " ")
        HTMLEntities.new.decode(str)
    end

    def fetchCaptions(xml_uri)
        doc = Nokogiri::HTML(open(xml_uri))

        doc.xpath("//transcript").children.reduce([]) do |lines, text|
            lines << {
                time: text.attributes["start"].value,
                text: format_caption_text(text.children.inner_text)
            }
        end
    end
end