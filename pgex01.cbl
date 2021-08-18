      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGEX01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Cadastro de estandes :                                     *
      *                                                             *
      *  Data da ultima alteracao:    15/12/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgex01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqex01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ex01-chave
                  alternate record key is ex01-chave-1 with duplicates
                  alternate record key is ex01-chave-2 with duplicates
                  file status is ex01-status.
      *
       data division.
       file section.
      *    
       copy fdex01.lib.
      *    
       working-storage section.
      *
       01 ex01-status                  pic x(02) value "00".
       01 ex01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ex01.
          02 ex01-dir                  pic x(03) value "EX2".
          02 filler                    pic x(01) value "\".
          02 ex01-nome                 pic x(08) value "ARQEX01A".
          02 filler                    pic x(01) value ".".
          02 ex01-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGEX01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 limpa-aux                    pic x(05) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 estande                   pic x(10) value spaces.
          02 codigo                    pic 9(05) value 0.
          02 codigo-disp               pic 9(05) value 0
             blank when zero.
          02 dcodigo                   pic x(40) value spaces.
          02 m2                        pic 9(04)v9(02) value 0.
          02 m2-aux                    pic z(03)9,9(02) value 0.
          02 m2-disp                   pic zzz9,99 value 0.
          02 setor                     pic 9(01) value 0.
          02 situacao                  pic x(01) value spaces.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
      *
       copy workgen.lib.
      * 
       linkage section.
      *
       01 param-menu.
          02 param-usr                 pic x(10).
          02 param-senha               pic x(10).
          02 param-prioridade          pic 9(01).
          02 param-data                pic 9(05).
          02 param-impress             pic x(12).
      *
       screen section.
      *
       01 tela-01.
          02 line 12 column 06 foreground-color 06 background-color 01
             highlight value "Estande...:".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "Expositor.:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Metragem..:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Setor.....:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Situacao..:".
      *
       01 tela-03.
          02 line 18 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 18 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 18 column 08 foreground-color 05 background-color 03
             value " - Expositores".
      *
       01 tela-04.
          02 line 11 column 65 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-05.
          02 line 11 column 65 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-06.
          02 line 18 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 18 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 18 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 18 column 16 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 18 column 18 foreground-color 05 background-color 03
             value "-Consultas".
      *
       01 tela-07.
          02 line 18 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 18 column 06 foreground-color 02 background-color 03 
             highlight value "1".
          02 line 18 column 07 foreground-color 05 background-color 03
             value " - Profissional".
          02 line 18 column 25 foreground-color 02 background-color 03 
             highlight value "2".
          02 line 18 column 26 foreground-color 05 background-color 03
             value " - Promocional".
      *
       01 tela-08.
          02 line 18 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 18 column 05 foreground-color 02 background-color 03
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-09.
          02 line 18 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 18 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 18 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 18 column 16 foreground-color 02 background-color 03
             highlight value "E".
          02 line 18 column 17 foreground-color 05 background-color 03
             value "stande".
          02 line 18 column 27 foreground-color 02 background-color 03
             highlight value "C".
          02 line 18 column 28 foreground-color 05 background-color 03
             value "odigo Espositor".
          02 line 18 column 47 foreground-color 02 background-color 03
             highlight value "M".
          02 line 18 column 48 foreground-color 05 background-color 03
             value "etragem".
      *
       01 tela-10.
          02 line 18 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 18 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 18 column 08 foreground-color 05 background-color 03
             value "-Alt".
          02 line 18 column 15 foreground-color 02 background-color 03 
             highlight value "F3".
          02 line 18 column 17 foreground-color 05 background-color 03
             value "-Exc".
          02 line 18 column 25 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 18 column 29 foreground-color 05 background-color 03
             value "-Inic".
          02 line 18 column 37 foreground-color 02 background-color 03
             highlight value "End".
          02 line 18 column 40 foreground-color 05 background-color 03
             value "-Fim".
          02 line 18 column 47 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 18 column 53 foreground-color 05 background-color 03
             value "-Prox".
          02 line 18 column 60 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 18 column 64 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-mensagem-cad.
          02 line 18 column 05 foreground-color 07 background-color 01
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 18 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 18 column 05 foreground-color 01 background-color 01
             pic x(68) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division using param-menu.
      *
       lab-00.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer.
           display tela-cabec.
           move 03 to box-col.
           move 09 to box-lin.
           move 72 to box-col-f.
           move 18 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-01.
      *
       lab-01.
           display tela-limpa-cad.
           perform sec-inclusao.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-rest-buffer.
      *
       lab-fim.
           exit program.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotinas section.
      *
       rot-move-ex01.
           move estande to ex01-estande.
           move 0 to ex01-codigo.
           move m2 to ex01-m2.
           move setor to ex01-setor.
           move "L" to ex01-situacao.
           move param-usr to ex01-usuario.
           move param-data to ex01-data.
      *
       rot-move-campos.
           move ex01-estande to estande.
           move ex01-codigo to codigo codigo-disp.
           if codigo = 0
              move spaces to dcodigo
           end-if.
           move ex01-m2 to m2 m2-disp m2-aux.
           move ex01-setor to setor.
           move ex01-situacao to situacao.
      *
       rot-display.
           perform rot-move-campos.
           perform dsp-estande thru dsp-situacao.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqex01 key is equal ex01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-ex01
           end-start.
      *
       rot-le-ex01-lock.
           move 0 to erro.
           read arqex01 next. 
           if ex01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ex01-lock
           end-if.
           read arqex01 with kept lock.
      *
      *
       rot-le-anterior.
           move 0 to erro.
           read arqex01 previous at end move 1 to erro.
           if ex01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqex01 next at end move 1 to erro.
           if ex01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ex01.
           move 0 to erro.
           if ex01-stat = "F"
              open i-o arqex01
              if ex01-status not = "00"
                 move 
                 " Erro de abertura no ARQEX01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
                 open output arqex01
                 move zeros to reg-ex01
                 move high-values to ex01-chave-controle
                 write reg-ex01-1
                 close arqex01
               else
                  move "A" to ex01-stat
               end-if
           end-if.
      *
       rot-close-ex01.
           if ex01-stat = "A"
              close arqex01
              move "F" to ex01-stat
           end-if.
      *
       rot-erro-leitura-ex01.
           move " Erro de leitura - ARQEX01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-ex01.
           move 0 to erro.
           read arqex01 invalid key move 1 to erro.
           if ex01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ex01
           end-if.
      *
       rot-inic-arquivo.
           perform lmp-estande thru lmp-situacao.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-estande thru lmp-situacao.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       err-estande-c.
           move " Estande ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       copy rotgen.lib.
      *
      ************************
      *                      *
      *       T e l a s      *
      *                      *
      ************************
      *
       telas section.
      *
       accept-resposta-cad.
           move spaces to resposta.
           accept resposta at 1868 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
      *  Sequencia para dar Accept
      *
       acc-estande.
           accept estande at 1218 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo.
           accept codigo at 1318 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-m2.
           accept m2-aux at 1418 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-setor.
           accept setor at 1518 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-situacao.
           accept situacao at 1618 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-estande.
           display estande at 1218 with foreground-color 15 
                   background-color 01.
      *
       dsp-codigo.
           display codigo-disp at 1318 with foreground-color 15 
                   background-color 01.
           display dcodigo at 1324 with foreground-color 15 
                   background-color 01.
      *
       dsp-m2.
           display m2-disp at 1418 with foreground-color 15
                   background-color 01.
      *
       dsp-setor.
           display setor at 1518 with foreground-color 15 
                   background-color 01.
      *
       dsp-situacao.
           display situacao at 1618 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-estande.
           display limpa at 1218 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo.
           display limpa at 1318 with foreground-color 15 
                   background-color 01.
      *
       lmp-m2.
           display limpa at 1418 with foreground-color 15 
                   background-color 01.
      *
       lmp-setor.
           display limpa at 1518 with foreground-color 15 
                   background-color 01.
      *
       lmp-situacao.
           display limpa at 1618 with foreground-color 15 
                   background-color 01.
      *
       sec-inclusao section.
      *
       lab-inc-00.
           display tela-limpa-cad.
           if param-prioridade < 1
              perform display-erro-usr
              go to lab-inc-fim
           end-if.
           perform rot-open-ex01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           display tela-04.
      *
       lab-inc-00-0.
           initialize campos.   
      *
       lab-inc-01.
           display tela-04.
           display tela-06.
           perform acc-estande.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform sec-consulta
              go to lab-inc-00-0
           end-if.
           if estande = spaces
              go to lab-inc-01
           end-if.
           move estande to ex01-estande.
           perform rot-le-ex01.
           if erro = 0
              perform err-estande-c
              go to lab-inc-01
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02.
           perform acc-m2.
           if escape-key = 1
              go to lab-inc-01
           end-if.
           move m2-aux to m2.
           if m2 = 0
              go to lab-inc-02
           end-if.
           move m2 to m2-disp.
           perform dsp-m2.
      *
       lab-inc-03.
           display tela-07
           perform acc-setor.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-02
           end-if.
           if setor not = 1 and 2
              go to lab-inc-03
           end-if.
      *
       lab-inc-04.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-03
           end-if.
           if resposta = "N"
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-04
              end-if
           end-if.
      *
       lab-inc-05.
           perform rot-move-ex01.
           write reg-ex01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQEX01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-08.
           perform rot-keypress.
           perform lmp-estande thru lmp-situacao.
           go to lab-inc-00-0.
      *
       lab-inc-fim.
           perform rot-close-ex01.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-limpa-cad.
           display tela-05.
      *
       lab-cns-01.
           display tela-09.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 69 or 101
                                 display tela-limpa-cad
                                 perform sec-consulta-estande
                                 display tela-09
                            when kbd2 = 67 or 99
                                 display tela-limpa-cad
                                 perform sec-consulta-codigo
                                 display tela-09
                            when kbd2 = 77 or 109
                                 display tela-limpa-cad
                                 perform sec-consulta-m2
                                 display tela-09
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           display tela-limpa.
           exit.
      *
       sec-consulta-estande section.
      *
       lab-cns-estande-00.
           move spaces to estande.
           perform lmp-estande.
           perform acc-estande.
           if escape-key = 1
              perform lmp-estande
              go to lab-cns-estande-fim
           end-if.
           display tela-10.
           move low-values to ex01-chave.
           move estande to ex01-estande.
      *
       lab-cns-estande-00-a.
           start arqex01 key is not less ex01-chave.
           go to lab-cns-estande-03.
      *
       lab-cns-estande-01.
           perform rot-le-anterior.
           if erro not = 0 or ex01-estande = estande
              perform rot-inic-arquivo
              start arqex01 key is not less ex01-chave
              move 1 to erro
              go to lab-cns-estande-05
           end-if.
           if ex01-chave = high-values
              go to lab-cns-estande-01
           end-if.
           go to lab-cns-estande-04.
      *
       lab-cns-estande-02.
           start arqex01 key is less ex01-chave.
      *
       lab-cns-estande-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ex01
              go to lab-cns-estande-fim
           end-if.
           if ex01-chave = high-values
              perform rot-fim-arquivo
              start arqex01 key is not less ex01-chave
              move low-values to estande
              move 1 to erro
              go to lab-cns-estande-05
           end-if.
      *
       lab-cns-estande-04.
           perform rot-display.
      *
       lab-cns-estande-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-estande-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-estande-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-estande-03
                    when kbd-aux = 73
                         go to lab-cns-estande-01
                    when kbd-aux = 71
                         move low-values to ex01-chave
                         go to lab-cns-estande-00-a
                    when kbd-aux = 79
                         move high-values to ex01-chave
                         go to lab-cns-estande-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-estande-05
           end-if.
           perform lmp-estande thru lmp-situacao.
           display tela-limpa-cad.
           go to lab-cns-estande-00.
      *
       lab-cns-estande-fim.
           move zeros to campo-kbd.
           perform lmp-estande thru lmp-situacao.
           exit.
      *
       sec-consulta-codigo section.
      *
       lab-cns-codigo-00.
           move 0 to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              go to lab-cns-codigo-fim
           end-if.
           display tela-10.
           move low-values to ex01-chave-1.
           move codigo to ex01-codigo.
      *
       lab-cns-codigo-00-a.
           start arqex01 key is not less ex01-chave-1.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or ex01-estande = estande
              perform rot-inic-arquivo
              start arqex01 key is not less ex01-chave-1
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if ex01-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqex01 key is less ex01-chave-1.
      *
       lab-cns-codigo-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ex01
              go to lab-cns-codigo-fim
           end-if.
           if ex01-chave = high-values
              perform rot-fim-arquivo
              start arqex01 key is not less ex01-chave-1
              move low-values to estande
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
      *
       lab-cns-codigo-04.
           perform rot-display.
      *
       lab-cns-codigo-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to ex01-chave-1
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to ex01-chave-1
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-estande thru lmp-situacao.
           display tela-limpa-cad.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-estande thru lmp-situacao.
           exit.
      *
       sec-consulta-m2 section.
      *
       lab-cns-m2-00.
           move 0 to m2-aux.
           perform lmp-m2.
           perform acc-m2.
           if escape-key = 1
              go to lab-cns-m2-fim
           end-if.
           move m2-aux to m2.
           move m2 to m2-disp.
           display tela-10.
           move low-values to ex01-chave-2.
           move m2 to ex01-m2 in ex01-chave-2.
      *
       lab-cns-m2-00-a.
           start arqex01 key is not less ex01-chave-2.
           go to lab-cns-m2-03.
      *
       lab-cns-m2-01.
           perform rot-le-anterior.
           if erro not = 0 or ex01-estande = estande
              perform rot-inic-arquivo
              start arqex01 key is not less ex01-chave-2
              move 1 to erro
              go to lab-cns-m2-05
           end-if.
           if ex01-chave = high-values
              go to lab-cns-m2-01
           end-if.
           go to lab-cns-m2-04.
      *
       lab-cns-m2-02.
           start arqex01 key is less ex01-chave-2.
      *
       lab-cns-m2-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ex01
              go to lab-cns-m2-fim
           end-if.
           if ex01-chave = high-values
              perform rot-fim-arquivo
              start arqex01 key is not less ex01-chave-2
              move low-values to estande
              move 1 to erro
              go to lab-cns-m2-05
           end-if.
      *
       lab-cns-m2-04.
           perform rot-display.
      *
       lab-cns-m2-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-m2-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-m2-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-m2-03
                    when kbd-aux = 73
                         go to lab-cns-m2-01
                    when kbd-aux = 71
                         move low-values to ex01-chave-2
                         go to lab-cns-m2-00-a
                    when kbd-aux = 79
                         move high-values to ex01-chave-2
                         go to lab-cns-m2-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-m2-05
           end-if.
           perform lmp-estande thru lmp-situacao.
           display tela-limpa-cad.
           go to lab-cns-m2-00.
      *
       lab-cns-m2-fim.
           move zeros to campo-kbd.
           perform lmp-estande thru lmp-situacao.
           exit.
      *
       sec-exclusao section.
       lab-exc-00-0.
           display tela-limpa-cad.
           if param-prioridade < 7
              perform display-erro-usr
              go to lab-exc-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-exc-fim
           end-if.
      *
       lab-exc-00.
           perform rot-le-ex01-lock.
           perform rot-display
           move "Excluir (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-exc-01.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-exc-fim
           end-if.
           if resposta = "N"
              go to lab-exc-fim
           else
              if resposta not = "S"
                 go to lab-exc-01
              end-if
           end-if.
           delete arqex01 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQEX01A.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
                  go to lab-exc-fim
           end-delete.
           move "Registro excluido - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-exc-fim.
           unlock arqex01 record.
           display tela-10.
           exit.
      *
       sec-alteracao section.
       lab-alt-00-0.
           display tela-limpa-cad.
           if param-prioridade < 5
              perform display-erro-usr
              go to lab-alt-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-alt-fim
           end-if.
      *
       lab-alt-00.
           perform rot-le-ex01-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform acc-m2.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move m2-aux to m2.
           if m2 = 0
              go to lab-alt-01
           end-if.
           move m2 to m2-disp.
           perform dsp-m2.
      *
       lab-alt-02.
           display tela-07
           perform acc-setor.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-01
           end-if.
           if setor not = 1 and 2
              go to lab-alt-02
           end-if.
      *
       lab-alt-03.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-02
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-03
              end-if
           end-if.
           perform rot-move-ex01.
           rewrite reg-ex01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQEX01A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-alt-fim
           end-rewrite.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-alt-fim.
           unlock arqex01 record.
           display tela-10.
           exit.