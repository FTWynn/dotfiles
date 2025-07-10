-- Learn about Neovim's lua api
-- https://neovim.io/doc/user/lua-guide.html

vim.o.number = true
--vim.o.tabstop = 2
--vim.o.shiftwidth = 2
vim.o.smartcase = true
vim.o.ignorecase = true
--vim.o.wrap = false
vim.o.hlsearch = false
vim.o.signcolumn = 'yes'

vim.cmd.colorscheme('habamax')

-- Space as the leader key
--vim.g.mapleader = ' ' -- for old nvim versions
vim.g.mapleader = vim.keycode('<Space>')

-- Basic clipboard interaction
vim.keymap.set({'n', 'x'}, 'gy', '"+y', {desc = 'Copy to clipboard'})
vim.keymap.set({'n', 'x'}, 'gp', '"+p', {desc = 'Paste clipboard text'})

-- Command shortcuts
vim.keymap.set('n', '<leader>w', '<cmd>write<cr>', {desc = 'Save file'})
vim.keymap.set('n', '<leader>q', '<cmd>quitall<cr>', {desc = 'Exit vim'})

