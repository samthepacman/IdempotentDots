-- DASHBOARD
local g = vim.g
local fn = vim.fn

g.dashboard_disable_at_vimenter = 0 -- dashboard is disabled by default
g.dashboard_disable_statusline = 1
g.dashboard_default_executive = "telescope"
g.dashboard_custom_header = {
   "                                   ",
   "                                   ",
   "                                   ",
   "   ⣴⣶⣤⡤⠦⣤⣀⣤⠆     ⣈⣭⣿⣶⣿⣦⣼⣆         ",
   "    ⠉⠻⢿⣿⠿⣿⣿⣶⣦⠤⠄⡠⢾⣿⣿⡿⠋⠉⠉⠻⣿⣿⡛⣦       ",
   "          ⠈⢿⣿⣟⠦ ⣾⣿⣿⣷    ⠻⠿⢿⣿⣧⣄     ",
   "           ⣸⣿⣿⢧ ⢻⠻⣿⣿⣷⣄⣀⠄⠢⣀⡀⠈⠙⠿⠄    ",
   "          ⢠⣿⣿⣿⠈    ⣻⣿⣿⣿⣿⣿⣿⣿⣛⣳⣤⣀⣀   ",
   "   ⢠⣧⣶⣥⡤⢄ ⣸⣿⣿⠘  ⢀⣴⣿⣿⡿⠛⣿⣿⣧⠈⢿⠿⠟⠛⠻⠿⠄  ",
   "  ⣰⣿⣿⠛⠻⣿⣿⡦⢹⣿⣷   ⢊⣿⣿⡏  ⢸⣿⣿⡇ ⢀⣠⣄⣾⠄   ",
   " ⣠⣿⠿⠛ ⢀⣿⣿⣷⠘⢿⣿⣦⡀ ⢸⢿⣿⣿⣄ ⣸⣿⣿⡇⣪⣿⡿⠿⣿⣷⡄  ",
   " ⠙⠃   ⣼⣿⡟  ⠈⠻⣿⣿⣦⣌⡇⠻⣿⣿⣷⣿⣿⣿ ⣿⣿⡇ ⠛⠻⢷⣄ ",
   "    ⢻⣿⣿⣄   ⠈⠻⣿⣿⣿⣷⣿⣿⣿⣿⣿⡟ ⠫⢿⣿⡆       ",
   "       ⠻⣿⣿⣿⣿⣶⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⡟⢀⣀⣤⣾⡿⠃     ",
   "                                   ",
}

-- g.dashboard_custom_header = {
--    "",
--    " ⠀⠀⠀  ⣶⣶⣶⣶⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀ ",
--    " ⠀ ⠀⠀⠀⠛⠛⢻⣿⣿⡀⠀⠀⠀⠀⠀⠀⠀⠀ ",
--    " ⠀⠀ ⠀⠀⠀⠀⢀⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀ ",
--    "  ⠀⠀⠀⠀⠀⢀⣾⣿⣿⣿⣇⠀⠀⠀⠀⠀⠀⠀ ",
--    "  ⠀⠀⠀⠀⢠⣿⣿⡟⢹⣿⣿⡆⠀⠀⠀⠀⠀⠀ ",
--    "   ⠀⠀⣰⣿⣿⠏⠀⠀⢻⣿⣿⡄⠀⠀⠀⠀⠀ ",
--    "  ⠀⠀⣴⣿⡿⠃⠀⠀⠀⠈⢿⣿⣷⣤⣤⡆⠀⠀ ",
--    "   ⠾⠿⠿⠁⠀⠀⠀⠀⠀⠘⣿⣿⡿⠿⠛⠀  ",
--    "",
--    "     Cool, Right?    ",
--    "",
-- }
g.dashboard_custom_section = {
   a = { description = { "  Find File                 SPC f f" }, command = "Telescope find_files" },
   b = { description = { "  Recents                   SPC f o" }, command = "Telescope oldfiles" },
   c = { description = { "  Find Word                 SPC f w" }, command = "Telescope live_grep" },
   d = { description = { "洛 New File                  SPC f n" }, command = "DashboardNewFile" },
   e = { description = { "  Bookmarks                 SPC b m" }, command = "Telescope marks" },
   f = { description = { "  Load Last Session         SPC l  " }, command = "SessionLoad" },
}

g.dashboard_custom_footer = {
   "   ",
   -- "NvChad Loaded " .. plugins_count .. " plugins",
   "NeoVim v0.5.0",
}

-- NVIM TREE
local present, tree_c = pcall(require, "nvim-tree.config")
if not present then
   return
end

require 'nvim-tree'.setup {}
local tree_cb = tree_c.nvim_tree_callback
local g = vim.g

vim.o.termguicolors = true

vim.opt.splitbelow = true                               -- Put new windows below current
vim.opt.splitright = true

g.nvim_tree_add_trailing = 0                            -- append a trailing slash to folder names
g.nvim_tree_allow_resize = 1
g.nvim_tree_auto_close = 0                              -- closes tree when it's the last window
g.nvim_tree_auto_ignore_ft = { "dashboard" }            -- don't open tree on specific fiypes.
g.nvim_tree_auto_open = 0
g.nvim_tree_disable_netrw = 1
g.nvim_tree_follow = 1
g.nvim_tree_git_hl = 1
g.nvim_tree_gitignore = 1
g.nvim_tree_hide_dotfiles = 0
g.nvim_tree_highlight_opened_files = 0
g.nvim_tree_hijack_netrw = 0
g.nvim_tree_indent_markers = 0
g.nvim_tree_ignore = { ".git", "node_modules", ".cache" }
g.nvim_tree_quit_on_open = 0 -- closes tree when file's opened
g.nvim_tree_root_folder_modifier = table.concat { ":t:gs?$?/..", string.rep(" ", 1000), "?:gs?^??" }
g.nvim_tree_side = "left"
g.nvim_tree_tab_open = 0
g.nvim_tree_update_cwd = 1
g.nvim_tree_width = 28
g.nvim_tree_lsp_diagnostics = 0

g.nvim_tree_show_icons = {
   folders = 1,
   folder_arrows = 1,
   files = 1,
   git = 1,
}

g.nvim_tree_icons = {
   default = " ",
   symlink = " ",
   git = {
      deleted = "",
      ignored = "◌",
      renamed = "➜",
      staged = "✓",
      unmerged = " ",
      unstaged = "✗",
      untracked = "★ ",
   },
   folder = {
      -- disable indent_markers option to get arrows working or if you want both arrows and indent then just add the arrow icons in front            ofthe default and opened folders below!
      arrow_open = "",
      arrow_closed = "",
--      arrow_open = "",
--      arrow_closed = "",
      default = " ",
      empty = " ", -- 
      empty_open = " ",
      open = " ",
      symlink = " ",
      symlink_open = " ",
   },
}



-- NvimTree
vim.cmd('autocmd BufWinEnter * :hi NvimTreeEmptyFolderName guibg=858a94')
vim.cmd('autocmd BufWinEnter * :hi NvimTreeEndOfBuffer guibg=616d85')
vim.cmd('autocmd BufWinEnter * :hi NvimTreeFolderIcon guibg=61afef')
vim.cmd('autocmd BufWinEnter * :hi NvimTreeGitDirty guibg=e08d69')
vim.cmd('autocmd BufWinEnter * :hi NvimTreeIndentMarker guibg=363c47')
vim.cmd('autocmd BufWinEnter * :hi NvimTreeNormal guibg=181b1f')

-- Disable some highlight in nvim tree if transparency enabled



-- local map = vim.api.nvim_set_keymap

require 'nvim-tree'.setup {}

vim.cmd('map <C-n> :NvimTreeToggle<CR>')
vim.cmd('let g:deoplete#enable_at_startup = 1')
vim.cmd('hi normal guibg=000000')
vim.cmd('map <C-j> :resize +7<CR>')
vim.cmd('map <C-k> :resize -7<CR>')
vim.cmd('map <C-h> :vertical resize +7<CR>')
vim.cmd('map <C-l> :vertical resize -7<CR>')
vim.g.mapleader = ' '
-- map ('n', 'C-h', ':horizontal resize +7<CR>', {noremap = true})
-- map ('n', 'C-l', ':horizontal resize -7<CR>', {noremap = true})
-- map ('n', 'C-j', ':vertical resize +7<CR>', {noremap = true})
-- map ('n', 'C-k', ':vertical resize -7<CR>', {noremap = true})

g.nvim_tree_bindings = {
   { key = { "<CR>", "o", "<2-LeftMouse>" }, cb = tree_cb "edit" },
   { key = { "<2-RightMouse>", "<C-]>" }, cb = tree_cb "cd" },
   { key = "<C-v>", cb = tree_cb "vsplit" },
   { key = "<C-x>", cb = tree_cb "split" },
   { key = "<C-t>", cb = tree_cb "tabnew" },
   { key = "<", cb = tree_cb "prev_sibling" },
   { key = ">", cb = tree_cb "next_sibling" },
   { key = "P", cb = tree_cb "parent_node" },
   { key = "<BS>", cb = tree_cb "close_node" },
   { key = "<S-CR>", cb = tree_cb "close_node" },
   { key = "<Tab>", cb = tree_cb "preview" },
   { key = "K", cb = tree_cb "first_sibling" },
   { key = "J", cb = tree_cb "last_sibling" },
   { key = "I", cb = tree_cb "toggle_ignored" },
   { key = "H", cb = tree_cb "toggle_dotfiles" },
   { key = "R", cb = tree_cb "refresh" },
   { key = "a", cb = tree_cb "create" },
   { key = "d", cb = tree_cb "remove" },
   { key = "r", cb = tree_cb "rename" },
   { key = "<C->", cb = tree_cb "full_rename" },
   { key = "x", cb = tree_cb "cut" },
   { key = "c", cb = tree_cb "copy" },
   { key = "p", cb = tree_cb "paste" },
   { key = "y", cb = tree_cb "copy_name" },
   { key = "Y", cb = tree_cb "copy_path" },
   { key = "gy", cb = tree_cb "copy_absolute_path" },
   { key = "[c", cb = tree_cb "prev_git_item" },
   { key = "}c", cb = tree_cb "next_git_item" },
   { key = "-", cb = tree_cb "dir_up" },
   { key = "q", cb = tree_cb "close" },
   { key = "g?", cb = tree_cb "toggle_help" },
}

--- LSP
local present1, lspconfig = pcall(require, "lspconfig")
local present2, lspinstall = pcall(require, "lspinstall")

if not (present1 or present2) then
   return
end

local function on_attach(_, bufnr)
   local function buf_set_keymap(...)
      vim.api.nvim_buf_set_keymap(bufnr, ...)
   end
   local function buf_set_option(...)
      vim.api.nvim_buf_set_option(bufnr, ...)
   end

   -- Enable completion triggered by <c-x><c-o>
   buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

   -- Mappings.
   local opts = { noremap = true, silent = true }

   -- See `:help vim.lsp.*` for documentation on any of the below functions
   buf_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
   buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
   buf_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
   buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
   buf_set_keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
   buf_set_keymap("n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
   buf_set_keymap("n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
   buf_set_keymap("n", "<space>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
   buf_set_keymap("n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
   buf_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
   buf_set_keymap("n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
   buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
   buf_set_keymap("n", "<space>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
   buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
   buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
   buf_set_keymap("n", "<space>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
   buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
   buf_set_keymap("v", "<space>ca", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

-- lspInstall + lspconfig stuff

local function setup_servers()
   lspinstall.setup()
   local servers = lspinstall.installed_servers()

   for _, lang in pairs(servers) do
      if lang ~= "lua" then
         lspconfig[lang].setup {
            on_attach = on_attach,
            capabilities = capabilities,
            -- root_dir = vim.loop.cwd,
         }
      elseif lang == "lua" then
         lspconfig[lang].setup {
            on_attach = on_attach,
            capabilities = capabilities,
            settings = {
               Lua = {
                  diagnostics = {
                     globals = { "vim" },
                  },
                  workspace = {
                     library = {
                        [vim.fn.expand "$VIMRUNTIME/lua"] = true,
                        [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
                     },
                     maxPreload = 100000,
                     preloadFileSize = 10000,
                  },
                  telemetry = {
                     enable = false,
                  },
               },
            },
         }
      end
   end
end

setup_servers()

-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
lspinstall.post_install_hook = function()
   setup_servers() -- reload installed servers
   vim.cmd "bufdo e"
end

-- replace the default lsp diagnostic symbols
local function lspSymbol(name, icon)
   vim.fn.sign_define("LspDiagnosticsSign" .. name, { text = icon, numhl = "LspDiagnosticsDefaul" .. name })
end

lspSymbol("Error", "")
lspSymbol("Information", "")
lspSymbol("Hint", "")
lspSymbol("Warning", "")

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
   virtual_text = {
      prefix = "",
      spacing = 0,
   },
   signs = true,
   underline = true,
   update_in_insert = false, -- update diagnostics insert mode
})
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
   border = "single",
})
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
   border = "single",
})

-- suppress error messages from lang servers
vim.notify = function(msg, log_level, _opts)
   if msg:match "exit code" then
      return
   end
   if log_level == vim.log.levels.ERROR then
      vim.api.nvim_err_writeln(msg)
   else
      vim.api.nvim_echo({ { msg } }, true, {})
   end
end
