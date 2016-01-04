require 'kramdown'

guide_md = `curl https://raw.githubusercontent.com/henrystanley/Quark/master/Doc/intro.md`
quark_commit_id = `git ls-remote "https://github.com/henrystanley/quark" HEAD`.split.first

guide_html = Kramdown::Document.new(guide_md).to_html
guide_template = File.read 'guide_template.html'
guide_rendered = guide_template
                   .gsub('{guide-goes-here}', guide_html)
                   .gsub('{commit-id-goes-here}', quark_commit_id)

File.open('guide.html', 'w+') { |f| f << guide_rendered }
