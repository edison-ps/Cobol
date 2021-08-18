      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGFT03       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Calculos Diversos :                                        *
      *                                                             *
      *  Data da ultima alteracao:    02/02/96     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgft03.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqab01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is automatic
                  with lock on record
                  record key is ab01-chave
                  alternate record key is ab01-chave-1 with duplicates
                  alternate record key is ab01-chave-2 with duplicates
                  file status is ab01-status.
      *
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
       copy fdab01.lib.
      *    
       copy fdrc01.lib.
      *    
       copy fdtabl.lib.
      *
       working-storage section.
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
          02 cb-programa               pic x(08) value "PGFT03".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 limpa-aux                    pic x(05) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 emissao                   pic 9(06) value 0.
          02 emissao-disp              pic x(08) value spaces.
          02 codigo                    pic 9(05) value 0.
          02 dcodigo                   pic x(40) value spaces.
          02 vencimento                pic 9(06) value 0.
          02 vencimento-disp           pic x(08) value spaces.
          02 valor-aux                 pic z(10)9,9(02) value 0.
          02 valor-disp                pic zz.zzz.zzz.zz9,99.
          02 documento                 pic x(10) value spaces.
          02 portador                  pic 9(03) value 0.
          02 dportador                 pic x(40) value spaces.
          02 operacao                  pic 9(03) value 0.
          02 doperacao                 pic x(40) value spaces.
          02 obs                       pic x(30) value spaces.
          02 cont-rateio               pic 9(06) value 0.
      *
       01 valor                        pic 9(11)v9(02) value 0.
      *
       01 valor-redef redefines valor.
          02 valor-inteiro             pic 9(11).
          02 valor-decimal             pic 9(02).
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
       01 campo-rotina-cod.
          02 rotina-col-cod            pic 9(02) value 0.
          02 rotina-lin-cod            pic 9(02) value 0.
          02 rotina-borda-cod          pic x(01) value spaces.
          02 rotina-fundo-cod          pic x(01) value spaces.
          02 rotina-sombra-cod         pic x(01) value spaces.
          02 rotina-codigo-cod         pic 9(05) value 0.
          02 rotina-condicao-cod       pic x(01) value spaces.
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
          02 line 12 column 56 foreground-color 06 background-color 01
             highlight value "Calculos Diversos".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "Emissao........:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Codigo.........:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Vencimento.....:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Valor..........:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Portador.......:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Operacao.......:".
          02 line 19 column 06 foreground-color 06 background-color 01
             highlight value "Observacao.....:".
      *
       01 tela-02.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Portadores".
      *
       01 tela-03.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Operacoes".
      *
       01 tela-04.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Associados".
      *
       01 tela-mensagem-cad.
          02 line 22 column 05 foreground-color 07 background-color 01
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 22 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 22 column 05 foreground-color 01 background-color 01
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
           move 10 to box-lin.
           move 72 to box-col-f.
           move 22 to box-lin-f.
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
           move spaces to rc01-doc-cob.
           move "A" to rc01-condicao.
           move codigo to rc01-codigo.
           move vencimento to rc01-vencimento.
           move obs to rc01-obs.
           move portador to rc01-portador.
           move operacao to rc01-operacao.
           move emissao to rc01-emissao.
           move param-usr to rc01-usuario.
           move param-data to rc01-data.
           move valor to rc01-valor.
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
       rot-le-ab01.
           move 0 to erro.
           read arqab01 invalid key move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab01
           end-if.
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
       rot-le-tabl.
           move 0 to erro.
           read arqtabl invalid key move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-tabl.
      *
       rot-pesq-associado.
           perform rot-close-ab01.
           move 08 to rotina-col-cod.
           move 12 to rotina-lin-cod.
           move "3" to rotina-borda-cod.
           move spaces to rotina-fundo-cod.
           move "S" to rotina-sombra-cod.
           move "A" to rotina-condicao-cod.
           call "rotab01" using param-menu campo-rotina-cod.
           cancel "rotab01".
           perform rot-open-ab01.
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
       err-codigo-n.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-cancelado.
           move " Associado cancelado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       msg-documento-g.
           move "Documento gravado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
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
           accept resposta at 2068 with auto foreground-color 01
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
           accept emissao at 1323 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo.
           accept codigo at 1423 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-vencimento.
           accept vencimento at 1523 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-valor.
           accept valor-aux at 1623 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-portador.
           accept portador at 1723 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-operacao.
           accept operacao at 1823 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-obs.
           accept obs at 1923 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-emissao.
           display emissao-disp at 1323 with foreground-color 15 
                   background-color 01.
      *
       dsp-codigo.
           display codigo at 1423 with foreground-color 15 
                   background-color 01.
           display dcodigo at 1429 with foreground-color 15 
                   background-color 01.
      *
       dsp-vencimento.
           display vencimento-disp at 1523 with foreground-color 15 
                   background-color 01.
      *
       dsp-valor.
           display valor-disp at 1623 with foreground-color 15 
                   background-color 01.
      *
       dsp-portador.
           display portador at 1723 with foreground-color 15 
                   background-color 01.
           display dportador at 1727 with foreground-color 15 
                   background-color 01.
      *
       dsp-operacao.
           display operacao at 1823 with foreground-color 15 
                   background-color 01.
           display doperacao at 1827 with foreground-color 15 
                   background-color 01.
      *
       dsp-obs.
           display obs at 1923 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-emissao.
           display limpa at 1323 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo.
           display limpa at 1423 with foreground-color 15 
                   background-color 01.
      *
       lmp-vencimento.
           display limpa at 1523 with foreground-color 15 
                   background-color 01.
      *
       lmp-valor.
           display limpa at 1623 with foreground-color 15 
                   background-color 01.
      *
       lmp-portador.
           display limpa at 1723 with foreground-color 15 
                   background-color 01.
      *
       lmp-operacao.
           display limpa at 1823 with foreground-color 15 
                   background-color 01.
      *
       lmp-obs.
           display limpa at 1923 with foreground-color 15 
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
           perform rot-open-ab01.
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
           move 0 to codigo.
      *
       lab-rat-01-1.
           display tela-04.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              display tela-limpa-cad
              go to lab-rat-01
           end-if.
           if escape-key = 3
              perform rot-pesq-associado
              move rotina-codigo-cod to codigo
              go to lab-rat-01-1
           end-if.
           if codigo = 0 
              go to lab-rat-01-1
           end-if.
           move "A" to ab01-condicao.
           move codigo to ab01-codigo.
           perform rot-le-ab01.
           if erro not = 0
              perform err-codigo-n
              go to lab-rat-01-1
           end-if.
           move ab01-razao-social-a to dcodigo.
           perform dsp-codigo.
           if ab01-situacao not = 1 and 5
              perform err-cancelado
              go to lab-rat-01-1
           end-if.
           display tela-limpa-cad.
      *
       lab-rat-02.
           move 0 to vencimento.
           perform lmp-vencimento.
           perform acc-vencimento.
           if escape-key = 1
              perform lmp-vencimento
              move 0 to codigo
              go to lab-rat-01-1
           end-if.
           if vencimento = 0
              move "C/APRES." to vencimento-disp
              perform dsp-vencimento
              go to lab-rat-03
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
      *
       lab-rat-03.
           move 0 to valor-aux.
           perform lmp-valor.
           perform acc-valor.
           if escape-key = 1
              perform lmp-valor
              go to lab-rat-02
           end-if.
           move valor-aux to valor.
           if valor = 0
              go to lab-rat-03
           end-if.
           move valor to valor-disp.
           perform dsp-valor.
      *
       lab-rat-04.
           move rotina-codigo to portador.
           display tela-02.
           perform lmp-portador.
           perform acc-portador.
           if escape-key = 1
              perform lmp-portador
              display tela-limpa-cad
              go to lab-rat-03
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
              perform lmp-obs
              go to lab-rat-05
           end-if.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-rat-07.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-rat-06
           end-if.
           if resposta = "N"
              perform lmp-emissao thru lmp-obs
              display tela-limpa-cad
              go to lab-rat-01
           else
              if resposta not = "S"
                 go to lab-rat-07
              end-if
           end-if.
           display tela-limpa-cad.
           move high-values to rc01-controle.
           perform rot-le-rc01-lock.
           if erro not = 0
              perform rot-erro-leitura-rc01
              go to lab-rat-fim
           end-if.
           add 1 to rc01-ult-fat.
           rewrite reg-rc01-1 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQRC01A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-rat-fim
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
                 go to lab-rat-fim
           end-write.
           unlock arqrc01 record.
           perform msg-documento-g.
           perform lmp-emissao thru lmp-obs.
           go to lab-rat-01.
      *
       lab-rat-fim.
           perform rot-close-ab01.
           perform rot-close-rc01.
           perform rot-close-tabl.
           exit.
      *
