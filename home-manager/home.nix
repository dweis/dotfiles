{ hiDpi, mainDisplay }:
{ config, pkgs, lib, ... }:

let
  config = {
    name = "Derrick Weis";
    email = "derrick@derrickweis.com";
    githubUsername = "dweis";
    color = {
      # Dracula - https://github.com/dracula/dracula-theme
      background = "#282a36";
      currentLine = "#44475a";
      selection = "#44475a";
      foreground = "#f8f8f2";
      comment = "#6272a4";
      cyan = "#8be9fd";
      green = "#50fa7b";
      orange = "#ffb86c";
      pink = "#ff79c6";
      purple = "#bd93f9";
      red = "#ff5555";
      yellow = "#f1fa8c";
      # extra
      darkGray = "#040404";
      white = "#bfbfbf";
      black = "#000000";
      blue = "#caa9fa";
      brightRed = "#ff6e67";
      brightGreen = "#5af78e";
      brightYellow = "#f4f99d";
      brightBlue = "#caa9fa";
      brightMagenta = "#ff92d0";
      brightCyan = "#9aedfe";
      brightWhite = "#e6e6e6";
    };
    fontSize = if hiDpi then 16 else 12;
    monospaceFont = "Hack";
    uiFont = "Noto Sans";
  };
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in with config; {
  fonts.fontconfig.enable = true;

  home = {
    packages = with pkgs; [
      # Dekstop
      xautolock
      i3lock-fancy
      # Fonts
      noto-fonts
      font-awesome-ttf
      material-icons
      powerline-fonts
      dejavu_fonts
      emojione
      # System utils
      fzf
      htop
      # Haskell
      (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
      (haskell.packages.ghc865.ghcWithPackages (p: with p; [
        stack
        cabal-install
        stylish-haskell
        hlint
        hoogle
      ]))
    ];

    file.".stack/config.yaml".text = ''
      templates:
        params:
          author-name: ${name}
          author-email: ${email}
          github-username: ${githubUsername}
          copyright: 'Copyright: (c) 2019 Derrick Weis'
      nix:
        enable: true
        packages: [zlib]
      system-ghc: true
    '';

    file.".ghci".text = ''
      :set editor ~/.nix-profile/bin/vi
    '';

    file.".haskeline".text = ''
      editMode: Vi
    '';

    file.".inputrc".text = ''
      set editing-mode vi
      set keymap vi
    '';

    file.".face".source = ./face.png;

    file.".config/rofi/dracula.rasi".text = ''
      // Dracula colors
      * {
          background: ${color.background};
          current-line: ${color.currentLine};
          selection: ${color.selection};
          foreground: ${color.foreground};
          comment: ${color.comment};
          cyan: ${color.cyan};
          green: ${color.green};
          orange: ${color.orange};
          pink: ${color.pink};
          purple: ${color.purple};
          red: ${color.red};
          yellow: ${color.yellow};
      }
      * {
          selected-normal-background:     @cyan;
          normal-background:              @background;
          normal-foreground:              @foreground;
          alternate-normal-background:    @normal-background;
          alternate-normal-foreground:    @foreground;
          selected-normal-foreground:     @normal-background;
          urgent-foreground:              @red;
          urgent-background:              @normal-background;
          alternate-urgent-background:    @urgent-background;
          alternate-urgent-foreground:    @urgent-foreground;
          selected-active-foreground:     @foreground;
          selected-urgent-background:     @normal-background;
          alternate-active-background:    @normal-background;
          alternate-active-foreground:    @selected-active-foreground;
          alternate-active-background:    @selected-active-background;
          border-color:                   @selected-normal-background;
          separatorcolor:                 @border-color;
          spacing: 2;
          background-color: @normal-background;
      }
      #window {
          border:           3;
          padding:          9;
      }
      #mainbox {
          background-color: inherit;
          border:  0;
          padding: 5;
      }
      #textbox {
          text-color: @foreground;
      }
      #element {
          border:  0;
          padding: 1px ;
      }
      #element.normal.normal {
          background-color: @normal-background;
          text-color:       @normal-foreground;
      }
      #element.normal.urgent {
          background-color: @urgent-background;
          text-color:       @urgent-foreground;
      }
      #element.normal.active {
          background-color: @active-background;
          text-color:       @active-foreground;
      }
      #element.selected.normal {
          background-color: @selected-normal-background;
          text-color:       @selected-normal-foreground;
      }
      #element.selected.urgent {
          background-color: @selected-urgent-background;
          text-color:       @selected-urgent-foreground;
      }
      #element.selected.active {
          background-color: @selected-active-background;
          text-color:       @selected-active-foreground;
      }
      #element.alternate.normal {
          background-color: @alternate-normal-background;
          text-color:       @alternate-normal-foreground;
      }
      #element.alternate.urgent {
          background-color: @alternate-urgent-background;
          text-color:       @alternate-urgent-foreground;
      }
      #element.alternate.active {
          background-color: @alternate-active-background;
          text-color:       @alternate-active-foreground;
      }
      #scrollbar {
          border:       0;
      }
      #button.selected {
          background-color: @selected-normal-background;
          text-color:       @selected-normal-foreground;
      }
      #inputbar {
          spacing:    10;
          text-color: @normal-foreground;
          background-color: inherit;
          padding:    1px ;
      }
      #case-indicator {
          spacing:    0;
          text-color: @normal-background;
      }
      #entry {
          spacing:    0;
          text-color: @normal-foreground;
      }
      #prompt {
          spacing:    0;
          text-color: @normal-foreground;
      }
    '';
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
            family = monospaceFont;
          };
          bold = {
            family = monospaceFont;
          };
          italic = {
            family = monospaceFont;
          };
          size = fontSize;
        };
        draw_bold_text_with_bright_colors = true;
        colors = {
          primary = {
            background = "${color.background}";
            foreground = "${color.foreground}";
          };
          normal = {
            black =   "${color.black}";
            red =     "${color.red}";
            green =   "${color.green}";
            yellow =  "${color.yellow}";
            blue =    "${color.blue}";
            magenta = "${color.purple}";
            cyan =    "${color.cyan}";
            white =   "${color.white}";
          };
          bright = {
            black =   "${color.background}";
            red =     "${color.brightRed}";
            green =   "${color.brightGreen}";
            yellow =  "${color.brightYellow}";
            blue =    "${color.brightBlue}";
            magenta = "${color.brightMagenta}";
            cyan =    "${color.brightCyan}";
            white =   "${color.brightWhite}";
          };
        };
        background_opacity = 0.87;
        visual_bell = {
          animation = "EaseOutExpo";
          duration = 0;
        };
        mouse_bindings = [ 
          {
          mouse = "Middle";
          action = "PasteSelection";
          }
        ];
        live_config_reload = true;
      };
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      userName = "${name}";
      userEmail = "${email}";
    };

    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
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

        colorscheme dracula

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

        " -- HIE
        let g:LanguageClient_serverCommands = { 'haskell': ['hie-wrapper'] }
        let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']
        hi link ALEError Error
        hi Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
        hi link ALEWarning Warning
        hi link ALEInfo SpellCap
        nnoremap <F5> :call LanguageClient_contextMenu()<CR>
        map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
        map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
        map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
        map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
        map <Leader>lb :call LanguageClient#textDocument_references()<CR>
        map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
        map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>
        '';

        plugins = with pkgs.vimPlugins; [
          vim-plug
          sensible
          airline
          vim-nix
          syntastic
          ctrlp
          deoplete-nvim
          fzf-vim
          editorconfig-vim
          awesome-vim-colorschemes
          nerdcommenter
          nerdtree
          supertab
          vim-scala
          vim-plug
          LanguageClient-neovim
          vim-stylish-haskell
          hlint-refactor-vim
          haskell-vim
          vim-haskellConcealPlus
        ];
    };

    rofi = {
      enable = true;
      terminal = "\${pkgs.alacritty}/bin/alacritty";
      theme = "dracula";
      font = "${monospaceFont} 16";
      extraConfig = ''
        rofi.modi: combi
        rofi.drun-icon-theme: Numix Square
        rofi.show-icons: true
      '';
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
        plugins = [ "aws" "ssh-agent" ];
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

  services.compton = {
    enable = true;
    backend = "glx";
    blur = true;
    shadow = true;
    activeOpacity = "0.98";
    inactiveOpacity = "0.90";
    menuOpacity = "0.95";
    fade = true;
    extraOptions = ''
      paint-on-overlay = true;
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
    '';
  };

  services.dunst = with pkgs; {
    enable = true;
    iconTheme = {
      package = numix-icon-theme-square;
      name = "Numix-Square";
      size = "48";
    };
    settings = {
      global = {
        format = "<b>%s</b>\\n%b";
        geometry = "600x7-20+20";
        transparency = 20;
        frame_width = 3;
        frame_color = color.blue;
        font = "${uiFont} ${toString (fontSize * 0.75)}";
        follow = "keyboard";
        max_icon_size = 64;
        icon_position = "left";
        browser = "google-chrome-stable";
        demnu = "rofi -dmenu -p dunst:";
      };
      frame = {
      };
      urgency_low = {
        background = color.background;
        foreground = color.foreground;
        frame_color = color.foreground;
        timeout = 5;
      };
      urgency_normal = {
        background = color.background;
        foreground = color.cyan;
        frame_color = color.cyan;
        timeout = 5;
      };
      urgency_critical = {
        background = color.background;
        foreground = color.red;
        frame_color = color.red;
        timeout = 20;
      };
    };
  };

  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3GapsSupport = true;
      alsaSupport = true;
      githubSupport = true;
      pulseSupport = true;
      jsoncpp = pkgs.jsoncpp;
    };
    config = {
      "settings" = {
        throttle-ms = 50;
        throttle-limit = 5;
      };
      "bar/main" = {
        monitor = "\${env:MONITOR:${mainDisplay}}";
        width = "100%";
        height = if hiDpi then "48" else "24";
        offset-y = 5;
        bottom = true;

        background = color.background;
        foreground = color.foreground;

        spacing = 1;
        padding-right = 2;
        module-margin-left = 0;
        module-margin-right = 2;

        font-0 = "${uiFont}:style=Bold,size=${toString fontSize};0";
        font-1 = "Material Icons:size=${toString (fontSize * 1.5)};4";
        font-2 = "FontAwesome:size=${toString (fontSize * 1.5)};4";
        font-3 = "EmojiOne Color:size=${toString (fontSize * 1.5)};4";

        modules-left = "i3";
        modules-center = "xwindow";
        modules-right = "cpu memory network1 network2 backlight battery pulseaudio clock";

        tray-position = "right";
        tray-maxsize = if hiDpi then 24 else 18;
        tray-scale = "1.0";
        tray-offset-y = 5;
        tray-offset-x = -1;
      };
      "module/backlight" = {
        type = "internal/backlight";
        format = "<ramp> <label>";
        card = "intel_backlight";
        ramp-0 = "🌔";
        ramp-1 = "🌔";
        ramp-2 = "🌓";
        ramp-3 = "🌒";
        ramp-4 = "🌑";
        ramp-foreground = color.orange;
        label-foreground = color.orange;
        bar-width = 10;
        bar-indicator = "|";
        bar-fill = "─";
        bar-empty = "─";
      };
      "module/date" = {
        type = "internal/date";
        internal = 5;
        date = "%d.%m.%y";
        time = "%H:%M";
        label = "%time%  %date%";
        label-foreground = color.yellow;
      };
      "module/i3" = {
        type = "internal/i3";
        format = "<label-state> <label-mode>";
        format-spacing = 0;
        label-focused = "👉  %index%";
        label-focused-padding = 2;
        label-focused-margin = 1;
        label-focused-font = 1;
        label-focused-foreground = color.pink;
        label-focused-background = color.darkGray;
        label-unfocused = "%index%";
        label-unfocused-padding = 2;
        label-unfocused-margin = 1;
        label-unfocused-foreground = color.foreground;
        label-unfocused-background = color.background;
        label-unfocused-font = 1;
        label-urgent = "%index%";
        label-urgent-padding = 2;
        label-urgent-margin = 1;
        label-urgent-font = 1;
        label-visible = "%index%";
        label-visible-padding = 2;
        label-visible-margin = 1;
        label-visible-font = 1;
        label-visible-foreground = color.cyan;
        index-sort = true;
      };
      "module/xwindow" = {
        type = "internal/xwindow";
        label-font = 1;
      };
      "module/cpu" = {
        type = "internal/cpu";
        interval = "0.5";
        format = "<label>";
        format-foreground = color.cyan;
        format-padding = 2;
        label = "  %percentage%%";
        label-font = 1;
      };
      "module/memory" = {
        type = "internal/memory";
        format = "<label>";
        label-font = 1;
        format-padding = 2;
        format-foreground = color.purple;
        label = " %gb_used%/%gb_total%";
      };
      "module/clock" = {
        type = "internal/date";
        format-padding = 2;
        label = "%time% %date%";
        time = " %H:%M %p";
        date = " %Y-%m-%d";
        label-foreground = color.pink;
      };
      "module/network1" = {
        type = "internal/network";
        interface = "enp5s0";
        label-connected = " %downspeed% %upspeed%";
        label-connected-foreground = color.orange;
      };
      "module/network2" = {
        type = "internal/network";
        interface = "wlp2s0";
        label-connected = " %downspeed% %upspeed%";
        label-connected-foreground = color.orange;
      };
      "module/battery" = {
        type = "internal/battery";
        battery = "BAT0";
        adapter = "AC";
        full-at = 99;
        padding = 2;

        time-format = "%H:%M";
        format-charging = "<animation-charging> <label-charging>";
        format-charging-foreground = color.yellow;
        label-charging = "%percentage%%";
        format-discharging = "<ramp-capacity> <label-discharging>";
        label-discharging = "%percentage%%";
        format-discharging-foreground = color.yellow;

        format-full = "<label-full>";
        format-full-foreground = color.yellow;
        format-full-prefix = " ";

        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";
        ramp-capacity-0-foreground = color.red;
        ramp-capacity-foreground = color.pink;
        bar-capacity-width = 10;
        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";

        animation-charging-framerate = 750;
      };
      "module/pulseaudio" = {
        type = "internal/pulseaudio";
        use-ui-max = true;
        interval = 5;

        format-volume = "<ramp-volume> <label-volume>";
        format-volume-foreground = color.green;
        format-volume-padding = 2;

        label-muted = " mute";
        label-foreground = color.green;

        label-volume = "%percentage%%";
        label-volume-foreground = color.green;
        label-muted-foreground = color.green;

        ramp-volume-0 = "";
        ramp-volume-1 = "";
        ramp-volume-2 = "";
        ramp-font = 1;
      };
    };
    script = "polybar main &";
  };

  services.random-background = {
    enable = true;
    imageDirectory = "%h/Wallpapers";
    interval = "1h";
  };

  services.redshift = {
    enable = true;
    latitude = "44.389";
    longitude = "-79.690";
  };

  xsession.windowManager.i3 = let
    modifier = "Mod4";
  in {
    enable = true;
    config = {
      bars = [];
      colors = {
        focused = { background = color.pink; border = color.pink; childBorder = color.pink; indicator = color.pink; text = color.foreground; };
        unfocused = { background = color.background; border = color.background; childBorder = color.darkGray; indicator = color.pink; text = color.foreground; };
        focusedInactive = { background = color.background; border = color.background; childBorder = color.pink; indicator = color.pink; text = color.foreground; };
        urgent = { background  = color.yellow; border  = color.yellow; childBorder = color.darkGray; indicator = color.pink; text = color.foreground; };
        placeholder = { background = color.background; border = color.background; childBorder = color.background; indicator = color.background; text = color.foreground; };
        background = color.background;
      };
      fonts = [ monospaceFont "FontAwesome 12" ];
      keybindings = lib.mkOptionDefault {
        "${modifier}+Shift+Return" = "exec alacritty";
        "${modifier}+Return" = "exec alacritty";
        "${modifier}+p" = ''exec "\${pkgs.rofi}/bin/rofi -show combi -lines 6"'';
        "${modifier}+x" = "kill";
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
        "${modifier}+Shift+q" = ''exec "i3-nagbar -t warning -m 'Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' 'i3-msg exit'"'';
        "${modifier}+Shift+z" = ''exec "i3lock-fancy"'';
        "${modifier}+r" = "mode resize";
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
        inner = 8;
        outer = 2;
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

          "Return" = "mode default";
          "Escape" = "mode default";
          "${modifier}+r" = "mode default";
        };
      };
      modifier = modifier;
      startup = [
        { command = "compton -b"; notification = false; }
        { command = "systemctl --user restart compton"; always = true; notification = false; }
        { command = "systemctl --user restart dunst"; always = true; notification = false; }
        { command = "systemctl --user restart polybar"; always = true; notification = false; }
        { command = "systemctl --user restart redshift"; always = true; notification = false; }
        { command = "systemctl --user restart random-background"; always = true; notification = false; }
        { command = "nm-applet"; notification = false; }
        { command = "xautolock -time 30 -locker i3lock-fancy"; always = true; notification = false; }
      ];
    };
    package = pkgs.i3-gaps;
  };
}
