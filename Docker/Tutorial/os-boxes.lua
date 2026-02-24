--[[
  os-boxes.lua — Pandoc Lua filter for OS-specific boxes

  Fenced-div classes handled:
    .os-compare  → side-by-side Windows + Mac boxes
    .windows     → full-width Windows box
    .mac         → full-width Mac box

  PDF output  → tcolorbox environments (defined in preamble.tex)
  HTML output → styled <div> cards

  We use a Blocks filter to walk the block list once, handling .os-compare
  parents BEFORE their children get transformed.  Standalone .windows/.mac
  divs (not nested inside .os-compare) are handled separately.
]]

local function is_latex(fmt)
  return fmt:match("latex") or fmt:match("pdf")
end

local function is_html(fmt)
  return fmt:match("html")
end

-- Render a Pandoc Blocks list to a raw string in the target format
local function blocks_to_raw(blocks, fmt)
  local doc = pandoc.Pandoc(blocks)
  return pandoc.write(doc, fmt)
end

function Div(el)
  local fmt = FORMAT

  -- ── .os-compare: side-by-side ──────────────────────────────────
  if el.classes:includes("os-compare") then
    local win_blocks = pandoc.Blocks{}
    local mac_blocks = pandoc.Blocks{}

    for _, child in ipairs(el.content) do
      if child.t == "Div" then
        if child.classes:includes("windows") then
          for _, b in ipairs(child.content) do
            win_blocks:insert(b)
          end
        elseif child.classes:includes("mac") then
          for _, b in ipairs(child.content) do
            mac_blocks:insert(b)
          end
        end
      end
    end

    if is_latex(fmt) then
      local win_inner = blocks_to_raw(win_blocks, "latex")
      local mac_inner = blocks_to_raw(mac_blocks, "latex")
      local raw = "\\noindent\n"
        .. "\\begin{minipage}[t]{0.48\\textwidth}\n"
        .. "\\begin{windowsbox}\n" .. win_inner .. "\n\\end{windowsbox}\n"
        .. "\\end{minipage}\\hfill\n"
        .. "\\begin{minipage}[t]{0.48\\textwidth}\n"
        .. "\\begin{macbox}\n" .. mac_inner .. "\n\\end{macbox}\n"
        .. "\\end{minipage}\n"
        .. "\\vspace{6pt}"
      return pandoc.RawBlock("latex", raw)

    elseif is_html(fmt) then
      local win_inner = blocks_to_raw(win_blocks, "html")
      local mac_inner = blocks_to_raw(mac_blocks, "html")
      local raw = '<div class="os-compare">'
        .. '<div class="os-box os-windows">'
        .. '<div class="os-box-header"><i class="fa-brands fa-windows"></i> Windows</div>'
        .. '<div class="os-box-body">' .. win_inner .. '</div></div>'
        .. '<div class="os-box os-mac">'
        .. '<div class="os-box-header"><i class="fa-brands fa-apple"></i> Mac</div>'
        .. '<div class="os-box-body">' .. mac_inner .. '</div></div>'
        .. '</div>'
      return pandoc.RawBlock("html", raw)
    end

    -- Return el unchanged for other formats
    return el
  end

  -- ── standalone .windows (not inside os-compare) ────────────────
  if el.classes:includes("windows") then
    if is_latex(fmt) then
      local inner = blocks_to_raw(el.content, "latex")
      return pandoc.RawBlock("latex",
        "\\begin{windowsbox}\n" .. inner .. "\n\\end{windowsbox}")
    elseif is_html(fmt) then
      local inner = blocks_to_raw(el.content, "html")
      return pandoc.RawBlock("html",
        '<div class="os-box os-windows">'
        .. '<div class="os-box-header"><i class="fa-brands fa-windows"></i> Windows</div>'
        .. '<div class="os-box-body">' .. inner .. '</div></div>')
    end
  end

  -- ── standalone .mac (not inside os-compare) ────────────────────
  if el.classes:includes("mac") then
    if is_latex(fmt) then
      local inner = blocks_to_raw(el.content, "latex")
      return pandoc.RawBlock("latex",
        "\\begin{macbox}\n" .. inner .. "\n\\end{macbox}")
    elseif is_html(fmt) then
      local inner = blocks_to_raw(el.content, "html")
      return pandoc.RawBlock("html",
        '<div class="os-box os-mac">'
        .. '<div class="os-box-header"><i class="fa-brands fa-apple"></i> Mac</div>'
        .. '<div class="os-box-body">' .. inner .. '</div></div>')
    end
  end

  -- pass through unchanged
  return nil
end

-- Ensure .os-compare is processed before its children by using
-- traverse = 'topdown' (Pandoc >= 2.17)
return {{
  traverse = 'topdown',
  Div = Div,
}}
