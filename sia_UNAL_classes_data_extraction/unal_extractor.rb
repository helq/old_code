require 'uri'
require 'net/http'
require 'nokogiri'
require 'pp'

def download_and_extract_info_from_sia(klass)
    url_post = URI.parse 'http://sia.bogota.unal.edu.co/academia/apoyo-administrativo/ConsultaContenidos.do'
    params = { 'action' => 'Info', 'idAsignatura' => klass }
    url_post.query = URI.encode_www_form params

    raw_page = Net::HTTP.get url_post
    page = Nokogiri::HTML raw_page

    attrs = {}
    page.css('div.zona-dato-caja:nth-child(6) > table > tr > td').each do |td|
        if td.children.size > 0
            h3 = td.css('h3').children[0].text
            content = td.children[4].text.strip
            attrs[ h3 ] = content
        end
    end

    attrs
end

klasses = ['1000044', '1000045', '1000046', '1000052', '1000053', '1000047', '1000002',
           '1000001', '1000074', '1000003', '2016696', '1000004', '1000006', '1000005',
           '1000007', '1000017', '1000019', '2015702', '2015703', '2015174', '2025963',
           '2025964', '2015970', '2015178', '1000013', '2016697', '2025983', '2016716',
           '2016353', '2016722', '2025960', '2016698', '2016699', '2016701', '2016702',
           '2025972', '2025975', '2025995', '2025966', '2025969', '2025970', '2025971',
           '2016703', '2015734', '2016375', '2025967', '2025982', '2016053', '2016707',
           '2024045', '2025994', '2016843', '2025973', '2025974']

klasses_attrs = {}
klasses.each do |id|
    klasses_attrs[id] = download_and_extract_info_from_sia id
#    p klasses_attrs[id]
end

PP.pp klasses_attrs

# showing classes that are NOT "validable"
#p klasses_attrs.select {|id, klass| klass['Validable']=='No'}.collect {|id, con| con["Nombre Asignatura"]}
