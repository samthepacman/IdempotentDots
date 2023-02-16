lua require 'init'
" exec 'so '.stdpath('config').'/xstl.vim'

aug RestoreCursorShapeOnExit
  au!
  au VimLeave,VimSuspend * set guicursor=a:ver30
  au VimEnter,VimResume * set guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20
aug END

let g:termbufm_code_scripts = {
      \ 'python': { 'build': [''],                                     'run': ['cat input | python %s', '%'] },
      \ 'cpp':    { 'build': ['g++ -std=c++11 -DFEAST_LOCAL %s', '%'], 'run': ['cat input | ./a.out'] },
      \ 'java':   { 'build': ['javac %s', '%'],                        'run': ['cat input | java %s', '%:r'] },
      \ 'c':      { 'build': ['gcc %s', '%'],                          'run': ['cat input | ./a.out'] },
      \ }

set fdm=marker fdl=0

set background=dark

nn <silent> <leader>b :call TermBufMExecCodeScript(&filetype, 'build')<CR>
nn <silent> <leader>r :call TermBufMExecCodeScript(&filetype, 'run')<CR>
nn <silent> <leader>f :call TermBufMExec('pbpaste > input')<CR>
nn <silent> <leader><space> :call TermBufMToggle()<CR>

let g:highlightedyank_highlight_duration = 300
runtime macros/sandwich/keymap/surround.vim
call operator#sandwich#set('all', 'all', 'highlight', 1)
