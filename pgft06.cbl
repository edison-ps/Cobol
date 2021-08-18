      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGFT06       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Rateio do Balcao :                                         *
      *                                                             *
      *  Data da ultima alteracao:    11/02/96     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgft02.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqrc01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is automatic
                  with lock on multiple records
                  record key is rc01-chave
                  alternate record key is rc01-chave-1 with duplicates
                  alternate record key is rc01-chave-2 with duplicates
                  alternate record key is rc01-chave-3 with duplicates
                  file status is rc01-status.
      *
           select arqab02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is automatic
                  with lock on record
                  record key is ab02-chave
                  alternate record key is ab02-chave-1 with duplicates
                  file status is ab02-status.
      *
           select arqtabl assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on record
                  record key is tabl-chave
                  alternate record key is tabl-chave-1 with duplicates
                  file status is tabl-status. 
      *
       data division.
       file section.
      *    
       copy fdrc01.lib.
      *    
       copy fdab02.lib.
      *
       copy fdtabl.lib.
      *
       working-storage section.
      *
       01 rc01-status                  pic x(02) value "00".
       01 rc01-stat                    pic x(01) value "F".
      *
       01 nome-arq-rc01.
          02 rc01-dir                  pic x(03) value "RC2".
          02 filler                    pic x(01) value "\".
          02 rc01-nome                 pic x(08) value "ARQRC01A".
          02 filler                    pic x(01) value ".".
          02 rc01-ext                  pic x(03) value "DAT".
      *
       01 ab02-status                  pic x(02) value "00".
       01 ab02-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab02.
          02 ab02-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab02-nome                 pic x(08) value "ARQAB02A".
          02 filler                    pic x(01) value ".".
          02 ab02-ext                  pic x(03) value "DAT".
      *
       01 tabl-status                  pic x(02) value "00".
       01 tabl-stat                    pic x(01) value "F".
      *
       01 nome-arq-tabl.
          02 tabl-dir                  pic x(03) value "TBL".
          02 filler                    pic x(01) value "\".
          02 tabl-nome                 pic x(08) value "ARQTABLA".
          02 filler                    pic x(01) value ".".
          02 tabl-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGFT06".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 limpa-13                     pic x(13) value spaces.
       01 limpa-aux                    pic x(05) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 emissao                   pic 9(06) value 0.
          02 emissao-disp              pic x(08) value spaces.
          02 vencimento                pic 9(06) value 0.
          02 vencimento-disp           pic x(08) value spaces.
          02 valor                     pic 9(07)v9(02) value 0.
          02 valor-aux                 pic z(7)9,9(02) value 0.
          02 valor-disp                pic z.zzz.zz9,99.
          02 documento                 pic x(10) value spaces.
          02 portador                  pic 9(03) value 0.
          02 dportador                 pic x(40) value spaces.
          02 operacao                  pic 9(03) value 0.
          02 doperacao                 pic x(40) value spaces.
          02 obs                       pic x(30) value spaces.
          02 cont-rateio               pic 9(06) value 0.
          02 codigo                    pic 9(06) value 0.
          02 razao                     pic x(40) value spaces.
          02 faixa-a1                  pic 9(07)v9(02) value 0.
          02 faixa-a2                  pic 9(07)v9(02) value 0.
          02 faixa-b1                  pic 9(07)v9(02) value 0.
          02 faixa-b2                  pic 9(07)v9(02) value 0.
          02 faixa-c                   pic 9(07)v9(02) value 0.
          02 faixa-d                   pic 9(07)v9(02) value 0.
          02 faixa-e                   pic 9(07)v9(02) value 0.
          02 faixa-f                   pic 9(07)v9(02) value 0.
          02 faixa-g                   pic 9(07)v9(02) value 0.
          02 faixa-h                   pic 9(07)v9(02) value 0.
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
       01 campo-rotina.
          02 rotina-col                pic 9(02) value 0.
          02 rotina-lin                pic 9(02) value 0.
          02 rotina-borda              pic x(01) value spaces.
          02 rotina-fundo              pic x(01) value spaces.
          02 rotina-sombra             pic x(01) value spaces.
          02 rotina-tipo               pic 9(02) value 0.
          02 rotina-codigo             pic 9(03) value 0.
      *
       copy workgen.lib.
       copy wstab01.lib.
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
          02 line 08 column 60 foreground-color 06 background-color 01
             highlight value "Rateio Balcao".
          02 line 09 column 06 foreground-color 06 background-color 01
             highlight value "Emissao........:".
          02 line 10 column 06 foreground-color 06 background-color 01
             highlight value "Vencimento.....:".
          02 line 11 column 06 foreground-color 06 background-color 01
             highlight value "Portador.......:".
          02 line 12 column 06 foreground-color 06 background-color 01
             highlight value "Operacao.......:".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "Observacao.....:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Faixa - A1.....:".
          02 line 14 column 40 foreground-color 06 background-color 01
             highlight value "Faixa - A2:....:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Faixa - B2.....:".
          02 line 15 column 40 foreground-color 06 background-color 01
             highlight value "Faixa - B2:....:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Faixa - C:.....:".
          02 line 16 column 40 foreground-color 06 background-color 01
             highlight value "Faixa - D:.....:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Faixa - E:.....:".
          02 line 17 column 40 foreground-color 06 background-color 01
             highlight value "Faixa - F:.....:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Faixa - G:.....:".
          02 line 18 column 40 foreground-color 06 background-color 01
             highlight value "Faixa - H:.....:".
      *
       01 tela-02.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value " - Portadores".
      *
       01 tela-03.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value " - Operacoes".
      *
       01 tela-mensagem-cad.
          02 line 21 column 05 foreground-color 07 background-color 01
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 21 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 21 column 05 foreground-color 01 background-color 01
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
           move 06 to box-lin.
           move 72 to box-col-f.
           move 21 to box-lin-f.
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
           perform sec-rateio.
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
       rot-move-rc01.
           move rc01-ult-fat to rc01-documento.
           move ab02-codigo to rc01-codigo.
           move spaces to rc01-doc-cob.
           move "A" to rc01-condicao.
           move vencimento to rc01-vencimento.
           move valor to rc01-valor.
           move obs to rc01-obs.
           move portador to rc01-portador.
           move operacao to rc01-operacao.
           move emissao to rc01-emissao.
           move param-usr to rc01-usuario.
           move param-data to rc01-data.
           move valor to rc01-valor.

           move ab02-codigo to codigo.
           move ab02-razao-social-a to razao.
           perform dsp-razao.
      *
       rot-le-rc01-lock.
           move 0 to erro.
           read arqrc01 invalid key move 1 to erro. 
           if rc01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-rc01-lock
           end-if.
      *
       rot-open-rc01.
           move 0 to erro.
           if rc01-stat = "F"
              open i-o arqrc01
              if rc01-status not = "00"
                 move 
                 " Erro de abertura no ARQRC01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to rc01-stat
               end-if
           end-if.
      *
       rot-close-rc01.
           if rc01-stat = "A"
              close arqrc01
              move "F" to rc01-stat
           end-if.
      *
       rot-erro-leitura-rc01.
           move " Erro de leitura - ARQRC01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-open-ab02.
           move 0 to erro.
           if ab02-stat = "F"
              open i-o arqab02
              if ab02-status not = "00"
                 move 
                 " Erro de abertura no ARQAB02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab02-stat
               end-if
           end-if.
      *
       rot-close-ab02.
           if ab02-stat = "A"
              close arqab02
              move "F" to ab02-stat
           end-if.
      *
       rot-pesq-tabela.
           perform rot-close-tabl.
           move 08 to rotina-col.
           move 12 to rotina-lin.
           move "3" to rotina-borda.
           move spaces to rotina-fundo.
           move "S" to rotina-sombra.
           call "pgtab01" using param-menu campo-rotina.
           cancel "pgtab01".
           perform rot-open-tabl.
      *
       rot-open-tabl.
           move 0 to erro.
           if tabl-stat = "F"
              open i-o arqtabl
              if tabl-status not = "00"
                 move 
                 " Erro de abertura no ARQTABLA.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
              else
                 move "A" to tabl-stat
              end-if
           end-if.
      *
       rot-close-tabl.
           if tabl-stat = "A"
              close arqtabl
              move "F" to tabl-stat
           end-if.
      *
       rot-le-ab02-lock.
           move 0 to erro.
           read arqab02 next at end move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab02-lock
           end-if.
      *
       rot-le-tabl.
           move 0 to erro.
           read arqtabl invalid key move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-tabl.
      *
       rot-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-data-m.
           move " Data do vencimento menor que emissao - Tecle <Enter>" 
           to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-erro-portador.
           move " Portador nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           
      *
       rot-erro-operacao.
           move " Operacao nao cadastrada - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
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
           accept resposta at 1968 with auto foreground-color 01
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
       acc-emissao.
           accept emissao at 0923 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-vencimento.
           accept vencimento at 1023 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-portador.
           accept portador at 1123 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-operacao.
           accept operacao at 1223 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-obs.
           accept obs at 1323 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-a1.
           accept valor-aux at 1423 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-a2.
           accept valor-aux at 1457 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-b1.
           accept valor-aux at 1523 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-b2.
           accept valor-aux at 1557 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-c.
           accept valor-aux at 1623 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-d.
           accept valor-aux at 1657 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-e.
           accept valor-aux at 1723 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-f.
           accept valor-aux at 1757 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-g.
           accept valor-aux at 1823 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa-h.
           accept valor-aux at 1857 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-emissao.
           display emissao-disp at 0923 with foreground-color 15 
                   background-color 01.
      *
       dsp-vencimento.
           display vencimento-disp at 1023 with foreground-color 15 
                   background-color 01.
      *
       dsp-portador.
           display portador at 1123 with foreground-color 15 
                   background-color 01.
           display dportador at 1127 with foreground-color 15 
                   background-color 01.
      *
       dsp-operacao.
           display operacao at 1223 with foreground-color 15 
                   background-color 01.
           display doperacao at 1227 with foreground-color 15 
                   background-color 01.
      *
       dsp-obs.
           display obs at 1323 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-a1.
           display valor-disp at 1423 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-a2.
           display valor-disp at 1457 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-b1.
           display valor-disp at 1523 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-b2.
           display valor-disp at 1557 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-c.
           display valor-disp at 1623 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-d.
           display valor-disp at 1657 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-e.
           display valor-disp at 1723 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-f.
           display valor-disp at 1757 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-g.
           display valor-disp at 1823 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa-h.
           display valor-disp at 1857 with foreground-color 15 
                   background-color 01.
      *
       dsp-razao.
           display codigo at 2006 with foreground-color 15 
                   background-color 01.
          display razao at 2013 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-emissao.
           display limpa at 0923 with foreground-color 15 
                   background-color 01.
      *
       lmp-vencimento.
           display limpa at 1023 with foreground-color 15 
                   background-color 01.
      *
       lmp-portador.
           display limpa at 1123 with foreground-color 15 
                   background-color 01.
      *
       lmp-operacao.
           display limpa at 1223 with foreground-color 15 
                   background-color 01.
      *
       lmp-obs.
           display limpa at 1323 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-a1.
           display limpa-13 at 1423 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-a2.
           display limpa-13 at 1457 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-b1.
           display limpa-13 at 1523 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-b2.
           display limpa-13 at 1557 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-c.
           display limpa-13 at 1623 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-d.
           display limpa-13 at 1657 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-e.
           display limpa-13 at 1723 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-f.
           display limpa-13 at 1757 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-g.
           display limpa-13 at 1823 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa-h.
           display limpa-13 at 1857 with foreground-color 15 
                   background-color 01.
      *
       lmp-razao.
           display limpa at 2005 with foreground-color 15 
                   background-color 01.
      *
       sec-rateio section.
      *
       lab-rat-00-0.
           display tela-limpa-cad.
           if param-prioridade < 8
              perform display-erro-usr
              go to lab-rat-fim
           end-if.
           perform rot-open-rc01.
           if erro not = 0
              go to lab-rat-fim
           end-if.
           perform rot-open-ab02.
           if erro not = 0
              go to lab-rat-fim
           end-if.
           perform rot-open-tabl.
           if erro not = 0
              go to lab-rat-fim
           end-if.
      *
       lab-rat-01.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to emissao emissao-disp.
           perform lmp-emissao.
           perform dsp-emissao.
           move 09 to box-lin.
           move 22 to box-col.
           call "C_Gotoxy" using by value box-col
                                 by value box-lin.
           perform rot-waitpress.
           if status-kbd not = 0
              go to lab-rat-fim
           end-if.
           perform acc-emissao.
           if escape-key = 1
              go to lab-rat-fim
           end-if.
           if emissao = 0
              go to lab-rat-01
           end-if.
           move emissao to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-rat-01
           end-if.
           move data-disp to emissao-disp.
           move dias-corr to emissao
           perform dsp-emissao.
      *
       lab-rat-02.
           move 0 to vencimento.
           perform lmp-vencimento.
           perform acc-vencimento.
           if escape-key = 1
              perform lmp-vencimento
              go to lab-rat-01
           end-if.
           if vencimento = 0
              move "C/APRES." to vencimento-disp
              perform dsp-vencimento
              move 0 to rotina-codigo
              go to lab-rat-04
           end-if.
           move vencimento to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-rat-02
           end-if.
           move data-disp to vencimento-disp.
           move dias-corr to vencimento
           perform dsp-vencimento.
           if emissao > vencimento
              perform rot-data-m
              go to lab-rat-02
           end-if.
           move 0 to rotina-codigo.
      *
       lab-rat-04.
           move rotina-codigo to portador.
           display tela-02.
           perform lmp-portador.
           perform acc-portador.
           if escape-key = 1
              perform lmp-portador
              display tela-limpa-cad
              go to lab-rat-02
           end-if.
           if escape-key = 3
              move 4 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-rat-04
           end-if.
           if portador = 0
              go to lab-rat-04
           end-if.
           move 04 to wtab01-tipo.
           move portador to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-portador
              go to lab-rat-04
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to dportador.
           perform dsp-portador.
           display tela-limpa-cad.
           move 0 to rotina-codigo.
      *
       lab-rat-05.
           move rotina-codigo to operacao.
           display tela-03.
           perform lmp-operacao.
           perform acc-operacao.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-operacao
              go to lab-rat-04
           end-if.
           if escape-key = 3
              move 5 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-rat-05
           end-if.
           if operacao = 0
              go to lab-rat-05
           end-if.
           move 05 to wtab01-tipo.
           move operacao to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-operacao
              go to lab-rat-05
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to doperacao.
           perform dsp-operacao.
           display tela-limpa-cad.
      *
       lab-rat-06.
           move spaces to obs.
           perform lmp-obs.
           perform acc-obs.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-obs
              go to lab-rat-05
           end-if.
      *
       lab-rat-07.
           move 0 to valor-aux.
           perform lmp-faixa-a1.
           perform acc-faixa-a1.
           if escape-key = 1
              perform lmp-faixa-a1
              go to lab-rat-06
           end-if.
           move valor-aux to faixa-a1 valor-disp.
           perform dsp-faixa-a1.
      *
       lab-rat-08.
           move 0 to valor-aux.
           perform lmp-faixa-a2.
           perform acc-faixa-a2.
           if escape-key = 1
              perform lmp-faixa-a2
              go to lab-rat-07
           end-if.
           move valor-aux to faixa-a2 valor-disp.
           perform dsp-faixa-a2.
      *
       lab-rat-09.
           move 0 to valor-aux.
           perform lmp-faixa-b1.
           perform acc-faixa-b1.
           if escape-key = 1
              perform lmp-faixa-b1
              go to lab-rat-08
           end-if.
           move valor-aux to faixa-b1 valor-disp.
           perform dsp-faixa-b1.
      *
       lab-rat-10.
           move 0 to valor-aux.
           perform lmp-faixa-b2.
           perform acc-faixa-b2.
           if escape-key = 1
              perform lmp-faixa-b2
              go to lab-rat-09
           end-if.
           move valor-aux to faixa-b2 valor-disp.
           perform dsp-faixa-b2.
      *
       lab-rat-11.
           move 0 to valor-aux.
           perform lmp-faixa-c.
           perform acc-faixa-c.
           if escape-key = 1
              perform lmp-faixa-c
              go to lab-rat-10
           end-if.
           move valor-aux to faixa-c valor-disp.
           perform dsp-faixa-c.
      *
       lab-rat-12.
           move 0 to valor-aux.
           perform lmp-faixa-d.
           perform acc-faixa-d.
           if escape-key = 1
              perform lmp-faixa-d
              go to lab-rat-11
           end-if.
           move valor-aux to faixa-d valor-disp.
           perform dsp-faixa-d.
      *
       lab-rat-13.
           move 0 to valor-aux.
           perform lmp-faixa-e.
           perform acc-faixa-e.
           if escape-key = 1
              perform lmp-faixa-e
              go to lab-rat-12
           end-if.
           move valor-aux to faixa-e valor-disp.
           perform dsp-faixa-e.
      *
       lab-rat-14.
           move 0 to valor-aux.
           perform lmp-faixa-f.
           perform acc-faixa-f.
           if escape-key = 1
              perform lmp-faixa-f
              go to lab-rat-13
           end-if.
           move valor-aux to faixa-f valor-disp.
           perform dsp-faixa-f.
      *
       lab-rat-15.
           move 0 to valor-aux.
           perform lmp-faixa-g.
           perform acc-faixa-g.
           if escape-key = 1
              perform lmp-faixa-g
              go to lab-rat-14
           end-if.
           move valor-aux to faixa-g valor-disp.
           perform dsp-faixa-g.
      *
       lab-rat-16.
           move 0 to valor-aux.
           perform lmp-faixa-h.
           perform acc-faixa-h.
           if escape-key = 1
              perform lmp-faixa-h
              go to lab-rat-15
           end-if.
           move valor-aux to faixa-h valor-disp.
           perform dsp-faixa-h.
      *
       lab-rat-17.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-rat-16
           end-if.
           if resposta = "N"
              perform lmp-emissao thru lmp-faixa-h
              display tela-limpa-cad
              go to lab-rat-01
           else
              if resposta not = "S"
                 go to lab-rat-17
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-rateio-01.
      *
       lab-rat-fim.
           perform rot-close-ab02.
           perform rot-close-rc01.
           perform rot-close-tabl.
           exit.
      *
       sec-rateio-01 section.
      *
       lab-rat-01-00.
           move 0 to erro.
           move low-values to ab02-chave.
           start arqab02 key is not less ab02-chave invalid key
                 move 1 to erro.
           if erro not = 0
              go to lab-rat-01-fim
           end-if.
      *
       lab-rat-01-01.
           move 0 to erro.
           perform rot-le-ab02-lock.
           if erro not = 0
              go to lab-rat-01-fim
           end-if.
           if ab02-chave = high-values
              go to lab-rat-01-01
           end-if.
           move 0 to valor.
           evaluate true
                    when ab02-faixa = "A1"
                         move faixa-a1 to valor
                    when ab02-faixa = "A2"
                         move faixa-a2 to valor
                    when ab02-faixa = "B"
                         move faixa-b1 to valor
                    when ab02-faixa = "B2"
                         move faixa-b2 to valor
                    when ab02-faixa = "C"
                         move faixa-c to valor
                    when ab02-faixa = "D"
                         move faixa-d to valor
                    when ab02-faixa = "E"
                         move faixa-e to valor
                    when ab02-faixa = "F"
                         move faixa-f to valor
                    when ab02-faixa = "G"
                         move faixa-g to valor
                    when ab02-faixa = "H"
                         move faixa-h to valor
           end-evaluate.
           move spaces to ab02-faixa.
           rewrite reg-ab02 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAB02A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-rat-01-fim
           end-rewrite.
           start arqab02 key is greater ab02-chave invalid key 
                 move 1 to erro
                 move " Erro de posicionamento - ARQAB02A.DAT - Tecle <E
      -          "nter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-rat-01-fim
           end-start.
           if valor = 0
              go to lab-rat-01-01
           end-if.
      *
       lab-rat-01-02.
           move high-values to rc01-controle.
           perform rot-le-rc01-lock.
           if erro not = 0
              perform rot-erro-leitura-rc01
              go to lab-rat-01-fim
           end-if.
      *
           add 1 to rc01-ult-fat.
           rewrite reg-rc01-1 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQRC01A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-rat-01-fim
           end-rewrite.
           unlock arqrc01 record.
           perform rot-move-rc01.
           write reg-rc01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQRC01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-rat-01-fim
           end-write.
           unlock arqrc01 record.
           go to lab-rat-01-01.
      *
       lab-rat-01-fim.
           perform lmp-razao.
           if cont-rateio not = 0 and erro = 0
              move "Faturamento efetuado - Tecle <Enter>" to mensagem
              display tela-mensagem-cad
              perform rot-keypress
           end-if.
           exit.