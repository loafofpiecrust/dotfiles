-- If you want pandoc to remove all divs and delete all IDs from its org-mode output,
-- use pandoc's `--lua-filter` option and pass it the following lua script.
-- For more information see https://pandoc.org/lua-filters.html

-- Keep headers from having extra gunk afterward.
function Header (elem)
  elem.identifier = ""
  return elem
end

-- Remove custom attributes applied to spans.
function Span (elem)
  return elem.content
end

-- Remove custom attributes applied to divs.
function Div (elem)
  return elem.content
end

-- Convert <br> elements into literal line breaks in org.
-- function LineBreak ()
--   return pandoc.Str("\n")
-- end

function Table (tb)
  if tb.rows == nil or tb.rows[1] == nil then
    return nil
  -- Unwrap tables with just a single cell.
  elseif tb.rows[2] == nil and tb.rows[1][2] == nil then
    -- blank = tb.rows[1][1] ~= nil and tb.rows[1][1].content:match("%S") ~= nil
    -- if blank then
    --   return nil
    -- else
      return tb.rows[1][1]
    -- end
  -- Unwrap tables with just a single column into paragraphs.
  elseif tb.rows[1][2] == nil then
    local result = {}
    for i, row in ipairs(tb.rows) do
      -- Divide each block with a line.
      if i ~= 1 then
        -- result[#result + 1] = pandoc.HorizontalRule()
      end
      for _, b in ipairs(row[1]) do
        result[#result + 1] = b
      end
    end
    return result
  else
    return tb
  end
end


-- Remove more empty paragraphs if possible.
function Para (elem)
  if elem.content[1] == nil or elem.content[1] == "" then
    return nil
  else
    return elem
  end
end
