

function Link(link)
  if link.attr and (link.attr.classes:includes("fileLink")) then
    local href = link.target
    local link_text = pandoc.utils.stringify(link.content)

    if FORMAT:match("html") or FORMAT:match("epub3") then
      return pandoc.RawInline("html",
        '<div style="text-align:right; font-size:0.85em; opacity:0.75">' ..
        '🗎 <a href="' .. href .. '">' .. link_text .. '</a>' ..
        '</div>'
      )
    end
    
    if FORMAT:match("latex") then
      return pandoc.RawInline("latex",
        "\\begin{flushright}\\footnotesize " ..
        "\\href{" .. href .. "}{" .. link_text .. "}" ..
        "\\end{flushright}"
      )
    end
  end
  
  return nil
end


function Image(image)
  if image.attr and (image.attr.classes:includes("dark-light")) then

    local srcDark = image.src
    local srcLight = image.src:gsub("dark", "light")
    local alt = "" --image.alt

    if FORMAT:match("html") then
      return pandoc.RawInline("html",
      '<picture>' ..
      '  <source media="(prefers-color-scheme: dark)" srcset="' .. srcDark .. '"/>' ..
      '  <source media="(prefers-color-scheme: light)" srcset="' .. srcLight .. '"/>' ..
      '  <img alt="' .. alt .. '" src="' .. srcDark .. '"/>' ..
      '</picture>'
      )
    end
  end
  return nil
end


-- Replace Unicode arrows with LaTeX equivalents for PDF output
function Str(str)
  if FORMAT:match("latex") then
    local text = str.text
    text = text:gsub("⟶", "$\\longrightarrow$")
    text = text:gsub("⟵", "$\\longleftarrow$")
    text = text:gsub("🔗", "Link:")
    if text ~= str.text then
      return pandoc.RawInline("latex", text)
    end
  end
  return nil
end
