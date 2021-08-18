      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  QGFT01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Calculo da mensalidade ABAV :                              *
      *                                                             *
      *  Data da ultima alteracao:    06/01/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. qgft01.
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
                  lock mode is exclusive
                  record key is rc01-chave
                  alternate record key is rc01-chave-1 with duplicates
                  alternate record key is rc01-chave-2 with duplicates
                  file status is rc01-status.
      *
           select arqab01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is exclusive
                  record key is ab01-chave
                  alternate record key is ab01-chave-1 with duplicates
                  alternate record key is ab01-chave-2 with duplicates
                  file status is ab01-status.
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
       copy fdab01.lib.
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
       01 ab01-status                  pic x(02) value "00".
       01 ab01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab01.
          02 ab01-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab01-nome                 pic x(08) value "ARQAB01A".
          02 filler                    pic x(01) value ".".
          02 ab01-ext                  pic x(03) value "DAT".
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
          02 cb-programa               pic x(08) value "QGFT01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
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
          02 valor-c                   pic 9(11)v9(02) value 0.
          02 valor-c-disp              pic zz.zzz.zzz.zz9,99.
          02 valor-i                   pic 9(11)v9(02) value 0.
          02 valor-i-disp              pic zz.zzz.zzz.zz9,99.
          02 valor-a                   pic 9(11)v9(02) value 0.
          02 valor-a-disp              pic zz.zzz.zzz.zz9,99.
          02 portador                  pic 9(03) value 0.
          02 dportador                 pic x(40) value spaces.
          02 operacao                  pic 9(03) value 0.
          02 doperacao                 pic x(40) value spaces.
          02 obs                       pic x(30) value spaces.
          02 razao                     pic x(40) value spaces.
      *
       01 documento.
          02 doc-1                     pic x(01) value spaces.
          02 doc-2                     pic x(02) value spaces.
          02 doc-3                     pic x(07) value spaces.
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
       01 buffer1.
          02 filler                    pic 9(04) occurs 2180.
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
          02 line 09 column 54 foreground-color 07 background-color 06
             highlight value "Calculo Mensalidade".
          02 line 10 column 06 foreground-color 02 background-color 06
             value "Emissao........:".
          02 line 11 column 06 foreground-color 02 background-color 06
             value "Vencimento.....:".
          02 line 12 column 06 foreground-color 02 background-color 06
             value "Valor Capital..:".
          02 line 13 column 06 foreground-color 02 background-color 06
             value "Valor Interior.:".
          02 line 14 column 06 foreground-color 02 background-color 06
             value "Valor Afiliado.:".
          02 line 15 column 06 foreground-color 02 background-color 06
             value "Portador.......:".
          02 line 16 column 06 foreground-color 02 background-color 06
             value "Operacao.......:".
          02 line 17 column 06 foreground-color 02 background-color 06
             value "Observacao.....:".
      *
       01 tela-02.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 02 
             highlight value "F2".
          02 line 20 column 08 foreground-color 01 background-color 02
             value " - Portadores".
      *
       01 tela-03.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 02 
             highlight value "F2".
          02 line 20 column 08 foreground-color 01 background-color 02
             value " - Operacoes".
      *
       01 tela-mensagem-cad.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 20 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 20 column 05 foreground-color 06 background-color 06
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
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer.
           display tela-cabec.
           move 03 to box-col.
           move 07 to box-lin.
           move 72 to box-col-f.
           move 20 to box-lin-f.
           move "3" to box-borda.
           move 06 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           call "C_Box" using by value box-col
                              by value box-lin
                              by value box-col-f
                              by value box-lin-f
                              by value box-cor-f
                              by value box-cor-p
                              by reference box-atrib.
           display tela-01.
       lab-01.
           display tela-limpa-cad.
           perform sec-fatura.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer.
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
           move ab01-condicao to doc-1.
           move "94" to doc-2.
           move ab01-codigo to doc-3.    
           move documento to rc01-documento.
           move ab01-codigo to rc01-codigo.
           move ab01-condicao to rc01-condicao.
           move vencimento to rc01-vencimento.
           move obs to rc01-obs.
           move portador to rc01-portador.
           move operacao to rc01-operacao.
           move emissao to rc01-emissao.
           move param-usr to rc01-usuario.
           move param-data to rc01-data.
           if ab01-condicao = "F"
              move valor-a to rc01-valor
           else
              if ab01-cep < 05901000
                 move valor-c to rc01-valor
              else
                 move valor-i to rc01-valor
              end-if
           end-if.
           move ab01-razao-social-a to razao.
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
       rot-open-ab01.
           move 0 to erro.
           if ab01-stat = "F"
              open i-o arqab01
              if ab01-status not = "00"
                 move 
                 " Erro de abertura no ARQAB01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab01-stat
               end-if
           end-if.
      *
       rot-close-ab01.
           if ab01-stat = "A"
              close arqab01
              move "F" to ab01-stat
           end-if.
      *
       rot-pesq-tabela.
           perform rot-close-tabl.
           move 08 to rotina-col.
           move 09 to rotina-lin.
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
       rot-le-ab01.
           move 0 to erro.
           read arqab01 next at end move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab01
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
           accept resposta at 1968 with auto foreground-color 06
                                             background-color 06.
           accept escape-key from escape.
           move resposta to txt.
           call "C_Text" using by reference campo-txt.
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
           accept emissao at 1023 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-vencimento.
           accept vencimento at 1123 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-valor-c.
           accept valor-c at 1223 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-valor-i.
           accept valor-i at 1323 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-valor-a.
           accept valor-a at 1423 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-portador.
           accept portador at 1523 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-operacao.
           accept operacao at 1623 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-obs.
           accept obs at 1723 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-emissao.
           display emissao-disp at 1023 with foreground-color 15 
                   background-color 06.
      *
       dsp-vencimento.
           display vencimento-disp at 1123 with foreground-color 15 
                   background-color 06.
      *
       dsp-valor-c.
           display valor-c-disp at 1223 with foreground-color 15 
                   background-color 06.
      *
       dsp-valor-i.
           display valor-i-disp at 1323 with foreground-color 15
                   background-color 06.
      *
       dsp-valor-a.
           display valor-a-disp at 1423 with foreground-color 15 
                   background-color 06.
      *
       dsp-portador.
           display portador at 1523 with foreground-color 15 
                   background-color 06.
           display dportador at 1527 with foreground-color 15 
                   background-color 06.
      *
       dsp-operacao.
           display operacao at 1623 with foreground-color 15 
                   background-color 06.
           display doperacao at 1627 with foreground-color 15 
                   background-color 06.
      *
       dsp-obs.
           display obs at 1723 with foreground-color 15 
                   background-color 06.
      *
       dsp-razao.
           display razao at 1905 with foreground-color 15 
                   background-color 06.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-emissao.
           display limpa at 1023 with foreground-color 15 
                   background-color 06.
      *
       lmp-vencimento.
           display limpa at 1123 with foreground-color 15 
                   background-color 06.
      *
       lmp-valor-c.
           display limpa at 1223 with foreground-color 15 
                   background-color 06.
      *
       lmp-valor-i.
           display limpa at 1323 with foreground-color 15 
                   background-color 06.
      *
       lmp-valor-a.
           display limpa at 1423 with foreground-color 15 
                   background-color 06.
      *
       lmp-portador.
           display limpa at 1523 with foreground-color 15 
                   background-color 06.
      *
       lmp-operacao.
           display limpa at 1623 with foreground-color 15 
                   background-color 06.
      *
       lmp-obs.
           display limpa at 1723 with foreground-color 15 
                   background-color 06.
      *
       lmp-razao.
           display limpa at 1905 with foreground-color 15 
                   background-color 06.
      *
       sec-fatura section.
      *
       lab-fat-00-0.
           display tela-limpa-cad.

           perform rot-open-tabl.
           if erro not = 0
              go to lab-fat-fim
           end-if.
      *
       lab-fat-01.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to emissao.
           perform lmp-emissao.
           perform acc-emissao.
           if escape-key = 1
              go to lab-fat-fim
           end-if.
           if emissao = 0
              go to lab-fat-01
           end-if.
           move emissao to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-fat-01
           end-if.
           move data-disp to emissao-disp.
           move dias-corr to emissao
           perform dsp-emissao.
      *
       lab-fat-02.
           move 0 to vencimento.
           perform lmp-vencimento.
           perform acc-vencimento.
           if escape-key = 1
              perform lmp-vencimento
              go to lab-fat-01
           end-if.
           if vencimento = 0
              move "C/APRES." to vencimento-disp
              perform dsp-vencimento
              go to lab-fat-03
           end-if.
           move vencimento to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-fat-02
           end-if.
           move data-disp to vencimento-disp.
           move dias-corr to vencimento
           perform dsp-vencimento.
           if emissao > vencimento
              perform rot-data-m
              go to lab-fat-02
           end-if.
      *
       lab-fat-03.
           move 0 to valor-c.
           perform lmp-valor-c.
           perform acc-valor-c.
           if escape-key = 1
              perform lmp-valor-c
              go to lab-fat-02
           end-if.
           if valor-c = 0
              go to lab-fat-03
           end-if.
           move valor-c to valor-c-disp.
           perform dsp-valor-c.
      *
       lab-fat-04.
           move 0 to valor-i.
           perform lmp-valor-i.
           perform acc-valor-i.
           if escape-key = 1
              perform lmp-valor-i
              go to lab-fat-03
           end-if.
           if valor-i = 0
              go to lab-fat-04
           end-if.
           move valor-i to valor-i-disp.
           perform dsp-valor-i.
      *
       lab-fat-05.
           move 0 to valor-a.
           perform lmp-valor-a.
           perform acc-valor-a.
           if escape-key = 1
              perform lmp-valor-a
              go to lab-fat-04
           end-if.
           if valor-a = 0
              go to lab-fat-05
           end-if.
           move valor-a to valor-a-disp.
           perform dsp-valor-a.
           move 0 to rotina-codigo.
      *
       lab-fat-06.
           move rotina-codigo to portador.
           display tela-02.
           perform lmp-portador.
           perform acc-portador.
           if escape-key = 1
              perform lmp-portador
              go to lab-fat-05
           end-if.
           if escape-key = 3
              move 4 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-fat-06
           end-if.
           if portador = 0
              go to lab-fat-06
           end-if.
           move 04 to wtab01-tipo.
           move portador to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-portador
              go to lab-fat-06
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to dportador.
           perform dsp-portador.
           display tela-limpa-cad.
           move 0 to rotina-codigo.
      *
       lab-fat-07.
           move rotina-codigo to operacao.
           display tela-03.
           perform lmp-operacao.
           perform acc-operacao.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-operacao
              go to lab-fat-06
           end-if.
           if escape-key = 3
              move 5 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-fat-07
           end-if.
           if operacao = 0
              go to lab-fat-07
           end-if.
           move 05 to wtab01-tipo.
           move operacao to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-operacao
              go to lab-fat-07
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to doperacao.
           perform dsp-operacao.
           display tela-limpa-cad.
      *
       lab-fat-08.
           move spaces to obs.
           perform lmp-obs.
           perform acc-obs.
           if escape-key = 1
              perform lmp-obs
              go to lab-fat-07
           end-if.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-fat-09.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-fat-08
           end-if.
           if resposta = "N"
              perform lmp-emissao thru lmp-obs
              go to lab-fat-01
           else
              if resposta not = "S"
                 go to lab-fat-09
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-fatura-01.
      *
       lab-fat-fim.
           perform rot-close-tabl.
           exit.
      *
       sec-fatura-01 section.
      *
       lab-fat-01-00.
           perform rot-open-rc01.
           if erro not = 0
              go to lab-fat-01-fim
           end-if.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-fat-01-fim
           end-if.
           move 0 to erro.
           move low-values to ab01-chave.
           start arqab01 key is not less ab01-chave invalid key
                 move 1 to erro.

      ***********

      *
       lab-fat-01-02.
           move 0 to erro.
           perform rot-le-ab01.
           if ab01-chave = high-values
              go to lab-fat-01-fim
           end-if.

      ***********

           if ab01-situacao not = 0
              go to lab-fat-01-02
           end-if.
           perform rot-move-rc01.
           write reg-rc01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQRC01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-fat-01-fim
           end-write.
           go to lab-fat-01-02.
      *
       lab-fat-01-fim.
           perform lmp-razao.
           perform rot-close-ab01.
           perform rot-close-rc01.
           exit.