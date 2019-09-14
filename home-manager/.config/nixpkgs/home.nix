{ config, pkgs, lib, ... }:

let
  name = "Derrick Weis";
  email = "derrick@derrickweis.com";
  githubUsername = "dweis";
  bgb = "#282a36";
  bg = "#282a36AA";
  fg = "#f8f8f2";
  tf = "#f8f8f2AA";
  hi = "#f1fa7c";
  tx = "#040404";
  pk = "#ff79c6";
  yw = "#f1fa7c";
  gn = "#50fa7b";
  rd = "#ff5555";
  id = "#ff79c6";
in {
  home = {
    packages = with pkgs; [
      rofi
    ];
  };

  programs = {
    home-manager.enable = true;

    alacritty = {
      enable = true;
      settings = {
        window = {
          dimensions = {
            columns = 80;
            lines = 24;
          };
        };
        font = {
          normal = {
            family = "Hack";
          };
          bold = {
            family = "Hack";
          };
          italic = {
            family = "Hack";
          };
          size = 14.0;
        };
        draw_bold_text_with_bright_colors = true;
        colors = {
          primary = {
            background = "0x282a36";
            foreground = "0xf8f8f2";
          };
          normal = {
            black =   "0x000000";
            red =     "0xff5555";
            green =   "0x50fa7b";
            yellow =  "0xf1fa8c";
            blue =    "0xcaa9fa";
            magenta = "0xff79c6";
            cyan =    "0x8be9fd";
            white =   "0xbfbfbf";
          };
          bright = {
            black =   "0x282a35";
            red =     "0xff6e67";
            green =   "0x5af78e";
            yellow =  "0xf4f99d";
            blue =    "0xcaa9fa";
            magenta = "0xff92d0";
            cyan =    "0x9aedfe";
            white =   "0xe6e6e6";
          };
        };
      };
    };

    git = {
      enable = true;
      userName = "${name}";
      userEmail = "${email}";
    };

    neovim = {
      viAlias = true;
      vimAlias = true;
      enable = true;
      extraConfig =
        ''
        set number
        set nowrap
        set showmode
        set smartcase
        set smartindent
        set softtabstop=2
        set shiftwidth=2
        set expandtab
        set history=1000
        set colorcolumn=80,100
        set list listchars=trail:.

        set completeopt=menuone,menu,longest

        set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox,output,node_modules,bower_components
        set wildmode=longest,list,full

        set t_Co=256

        set cmdheight=1

        colorscheme seoul256

        " -- Supertab
        let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

        if has("gui_running")
          imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
        else " no gui
          if has("unix")
            inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
          endif
        endif

        " -- NERDtree
        map <Leader>n :NERDTreeToggle<CR>

        " -- Ctrl-P
        map <silent> <Leader>t :CtrlP()<CR>
        noremap <leader>b<space> :CtrlPBuffer<cr>
        let g:ctrlp_custom_ignore = '\v[\/]dist$'

        " -- Airline
        let g:airline_powerline_fonts = 1
        '';

      plugins = with pkgs.vimPlugins; [
        sensible
        airline
        vim-nix
        syntastic
        ctrlp
        deoplete-nvim
        editorconfig-vim
        nerdcommenter
        nerdtree
        seoul256-vim
        supertab
        vim-scala
      ];
    };

    tmux = {
      enable = true;
      keyMode = "vi";
      terminal = "tmux-256color";
    };

    zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        theme = "norm";
        plugins = [ "aws" ];
      };
      envExtra = ''
        EDITOR="vim"
        TERM="xterm-256color"
      '';
      shellAliases = {
        fonts = "fc-list | cut -f2 -d: | sort -u";
        nix-search = "nix-env -qaP '*' | grep";
        nix-cleanup = "nix-collect-garbage -d && nix-store --optimize";
      };
    };
  };

  xsession.windowManager.i3 = let
    modifier = "Mod4";
  in {
    enable = true;
    config = {
      bars = [];
      colors = {
        focused = { background = id; border = pk; childBorder = tx; indicator = id; text = "#ffffff"; };
        unfocused = { background = bg; border = bg; childBorder = tf; indicator = id; text = "#ffffff"; };
        focusedInactive = { background = bg; border = bg; childBorder = pk; indicator = id; text = "#ffffff"; };
        urgent = { background = yw; border = yw; childBorder = tx; indicator = id; text = "#ffffff"; };
        placeholder = { background = bg; border = bg; childBorder = bg; indicator = bg; text = "#ffffff"; };
      };
      fonts = [ "Hack" "FontAwesome 12" ];
      keybindings = lib.mkOptionDefault {
        "${modifier}+Shift+Return" = "exec alacritty";
        "${modifier}+Return" = "emacs";
        "${modifier}+x" = "kill";
        "${modifier}+p" = ''exec "\${pkgs.rofi}/bin/rofi -show combi -lines 6"'';
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";
        "${modifier}+Left" = "focus left";
        "${modifier}+Down" = "focus down";
        "${modifier}+Up" = "focus up";
        "${modifier}+Right" = "focus right";
        "${modifier}+Shift+h" = "move left";
        "${modifier}+Shift+j" = "move down";
        "${modifier}+Shift+k" = "move up";
        "${modifier}+Shift+l" = "move right";
        "${modifier}+Shift+Left" = "move left";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Right" = "move right";
        "${modifier}+Tab" = "workspace back_and_forth";
        "${modifier}+q" = "reload";
        "${modifier}+Shift+r" = "restart";
        "${modifier}+Shift+q" = ''exec "mate-session-save --logout-dialog"'';
        "${modifier}+Shift+z" = ''exec "mate-screensaver-command -l"'';
        "${modifier}+r" = ''mode "resize"'';
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+s" = "layout stacking";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+t" = "layout toggle all";
        "${modifier}+Shift+space" = "floating toggle";
        "${modifier}+space" = "focus mode_toggle";
        "${modifier}+Ctrl+4" = ''exec "--no-startup-id mate-screenshot --interactive"'';
        "${modifier}+a" = "focus parent";
        "${modifier}+d" = "focus child";
        "XF86AudioRaiseVolume" = ''exec "amixer -q set Master 5%+"'';
        "XF86AudioLowerVolume" = ''exec "amixer -q set Master 5%-"'';
      };
      gaps = {
        inner = 4;
        outer = 4;
        smartBorders = "on";
      };
      modes = {
        resize = {
          "h" = "resize shrink width 10 px or 10 ppt";
          "j" = "resize grow height 10 px or 10 ppt";
          "k" = "resize shrink height 10 px or 10 ppt";
          "l" = "resize grow width 10 px or 10 ppt";

          "Left" = "resize shrink width 10 px or 10 ppt";
          "Down" = "resize grow height 10 px or 10 ppt";
          "Up" = "resize shrink height 10 px or 10 ppt";
          "Right" = "resize grow width 10 px or 10 ppt";

          "Return" = ''mode "default"'';
          "Escape" = ''mode "default"'';
          "${modifier}+r" = ''mode "default"'';
        };
      };
      modifier = modifier;
      startup = [
        { command = "compton -b"; notification = false; }
      ];
    };
  };
}
