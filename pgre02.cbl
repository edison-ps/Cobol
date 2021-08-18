      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGRE02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Baixa de CTAS a Receber :                                  *
      *                                                             *
      *  Data da ultima alteracao:    14/12/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgrc02.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqbe01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is be01-chave
                  alternate record key is be01-chave-1 with duplicates
                  alternate record key is be01-chave-2 with duplicates
                  file status is be01-status.
      *
           select arqre01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is re01-chave
                  alternate record key is re01-chave-1 with duplicates
                  alternate record key is re01-chave-2 with duplicates
                  file status is re01-status.
      *
           select arqab01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
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
       copy fdbe01.lib.
      *    
       copy fdre01.lib.
      *    
       copy fdab01.lib.
      *
       copy fdtabl.lib.
      *
       working-storage section.
      *
       01 be01-status                  pic x(02) value "00".
       01 be01-stat                    pic x(01) value "F".
      *
       01 nome-arq-be01.
          02 be01-dir                  pic x(03) value "RE2".
          02 filler                    pic x(01) value "\".
          02 be01-nome                 pic x(08) value "ARQBE01A".
          02 filler                    pic x(01) value ".".
          02 be01-ext                  pic x(03) value "DAT".
      *
       01 re01-status                  pic x(02) value "00".
       01 re01-stat                    pic x(01) value "F".
      *
       01 nome-arq-re01.
          02 re01-dir                  pic x(03) value "RE2".
          02 filler                    pic x(01) value "\".
          02 re01-nome                 pic x(08) value "ARQRE01A".
          02 filler                    pic x(01) value ".".
          02 re01-ext                  pic x(03) value "DAT".
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
          02 cb-programa               pic x(08) value "PGRE02".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 limpa-10                     pic x(10) value spaces.
       01 limpa-aux                    pic x(05) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 baixa                     pic 9(06) value 0.
          02 condicao                  pic x(01) value spaces.
          02 codigo                    pic 9(05) value 0.
          02 razao-social              pic x(40) value spaces.
          02 documento                 pic x(10) value spaces.
          02 vencimento                pic 9(06) value 0.
          02 vencimento-disp           pic x(08) value spaces.
          02 valor-aux                 pic z(10)9,9(02) value 0.
          02 valor                     pic 9(11)v9(02) value 0.
          02 valor-disp                pic zz.zzz.zzz.zz9,99.
          02 obs                       pic x(30) value spaces.
          02 portador                  pic 9(03) value 0.
          02 dportador                 pic x(40) value spaces.
          02 operacao                  pic 9(03) value 0.
          02 doperacao                 pic x(40) value spaces.
          02 valor-pago                pic 9(11)v9(02) value 0.
          02 valor-pago-disp           pic zz.zzz.zzz.zz9,99.
          02 liquidacao                pic 9(06) value 0.
          02 liquidacao-disp           pic x(08) value spaces.
          02 flag-re01                 pic x(01) value spaces.
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
          02 line 08 column 06 foreground-color 06 background-color 01
             highlight value "Baixa......:".
          02 line 09 column 06 foreground-color 06 background-color 01
             highlight value "Condicao...:".
          02 line 10 column 06 foreground-color 06 background-color 01
             highlight value "Codigo.....:".
          02 line 11 column 06 foreground-color 06 background-color 01
             highlight value "Documento..:".
          02 line 12 column 06 foreground-color 06 background-color 01
             highlight value "Vencimento.:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Valor......:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Observacao.:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Portador...:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Operacao...:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Valor Pago.:".
          02 line 19 column 06 foreground-color 06 background-color 01
             highlight value "Liquidacao.:".
      *
       01 tela-02.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 21 column 16 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 18 foreground-color 05 background-color 03
             value "-Consultas".
      *
       01 tela-03.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 21 column 16 foreground-color 02 background-color 03
             highlight value "B".
          02 line 21 column 17 foreground-color 05 background-color 03
             value "aixa".
          02 line 21 column 27 foreground-color 02 background-color 03
             highlight value "C".
          02 line 21 column 28 foreground-color 05 background-color 03
             value "odigo".
          02 line 21 column 39 foreground-color 02 background-color 03
             highlight value "L".
          02 line 21 column 40 foreground-color 05 background-color 03
             value "iquidacao".
      *
       01 tela-04.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Alt".
          02 line 21 column 15 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 21 column 19 foreground-color 05 background-color 03
             value "-Inic".
          02 line 21 column 26 foreground-color 02 background-color 03
             highlight value "End".
          02 line 21 column 29 foreground-color 05 background-color 03
             value "-Fim".
          02 line 21 column 36 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 21 column 42 foreground-color 05 background-color 03
             value "-Prox".
          02 line 21 column 50 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 21 column 54 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-05.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value " - Associados".
      *
       01 tela-06.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight value "Baixa Ok - Tecle <Enter>".
      *
       01 tela-07.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value " - Portadores".
      *
       01 tela-08.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value " - Operacoes".
      *
       01 tela-09.
          02 line 07 column 65 foreground-color 06 background-color 01
             highlight value "   Baixa".
      *
       01 tela-10.
          02 line 07 column 65 foreground-color 06 background-color 01
             highlight value "Consulta".
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
           move 05 to box-lin.
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
           perform sec-baixa.
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
       rot-move-be01.
           move baixa to be01-baixa.
           move codigo to be01-codigo.
           move condicao to be01-condicao.
           move documento to be01-documento.
           move vencimento to be01-vencimento.
           move valor to be01-valor.
           move obs to be01-obs.
           move portador to be01-portador.
           move operacao to be01-operacao.
           move valor-pago to be01-valor-pago.
           move liquidacao to be01-liquidacao.
           move param-usr to be01-usuario.
           move param-data to be01-data.      
      *
       rot-move-campos-be01.
           move be01-baixa to baixa.
           move be01-codigo to codigo.
           move be01-condicao to condicao.
           move be01-documento to documento.
           if be01-vencimento not = 0
              move be01-vencimento to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to vencimento-disp
           else
              move "C/APRES." to vencimento-disp
           end-if.
           move be01-vencimento to vencimento.
           move be01-valor to valor valor-disp.
           move be01-obs to obs.
           move be01-portador to portador.
           move be01-operacao to operacao.
           move be01-valor-pago to valor-pago valor-pago-disp valor-aux.
           move be01-liquidacao to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to liquidacao-disp.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to liquidacao.
           move be01-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move be01-usuario to cab-usuario.
      *
       rot-move-campos-re01.
           move re01-documento to documento.
           if re01-vencimento not = 0
              move re01-vencimento to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to vencimento-disp
           else
              move "C/APRES." to vencimento-disp
           end-if.
           move re01-vencimento to vencimento.
           move re01-valor to valor valor-disp.
           move re01-obs to obs.
           move re01-portador to portador.
           move re01-operacao to operacao.
           move re01-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move re01-usuario to cab-usuario.
           perform rot-descricao.
      *
       rot-descricao.
           move codigo to ab01-codigo.
           move condicao to ab01-condicao.
           move 0 to erro.
           perform rot-le-ab01.
           if erro not = 0
              move "Codigo nao cadastrado"  to razao-social
           else
              move ab01-razao-social-a to razao-social
           end-if.
           move 04 to wtab01-tipo.
           move portador to wtab01-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              move "Portador nao cadastrado" to dportador
           else
              move reg-tabl to reg-wtab01
              move wtab01-descricao to dportador
           end-if.
           move 05 to wtab01-tipo.
           move operacao to wtab01-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              move "Operacao nao cadastrada" to doperacao
           else
              move reg-tabl to reg-wtab01
              move wtab01-descricao to doperacao
           end-if.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqbe01 previous at end move 1 to erro.
           if re01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqbe01 next at end move 1 to erro.
           if re01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-le-be01.
           move 0 to erro.
           read arqbe01 invalid key move 1 to erro.
           if be01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-be01.
      *
       rot-ponteiro-be01.
           move 0 to erro.
           start arqbe01 key is equal be01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-be01
           end-start.
      *
       rot-le-be01-lock.
           move 0 to erro.
           read arqbe01 next. 
           if be01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-be01-lock
           end-if.
           read arqbe01 with kept lock.
      *
       rot-open-be01.
           move 0 to erro.
           if be01-stat = "F"
              open i-o arqbe01
              if be01-status not = "00"
                 move 
                 " Erro de abertura no ARQBE01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
                 open output arqbe01
                 move zeros to reg-be01-1
                 move high-values to be01-controle
                 move 0 to be01-ult-bx
                 write reg-be01-1
               else
                  move "A" to be01-stat
               end-if
           end-if.
      *
       rot-close-be01.
           if be01-stat = "A"
              close arqbe01
              move "F" to be01-stat
           end-if.
      *
       rot-erro-leitura-be01.
           move " Erro de leitura - ARQBE01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-re01.
           move 0 to erro.
           read arqre01 invalid key move 1 to erro.
           if re01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-re01.
      *
       rot-ponteiro-re01.
           move 0 to erro.
           start arqre01 key is equal re01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-re01
           end-start.
      *
       rot-le-re01-lock.
           move 0 to erro.
           read arqre01 next. 
           if re01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-re01-lock
           end-if.
           read arqre01 with kept lock.
      *
       rot-open-re01.
           move 0 to erro.
           if re01-stat = "F"
              open i-o arqre01
              if re01-status not = "00"
                 move 
                 " Erro de abertura no ARQRE01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to re01-stat
               end-if
           end-if.
      *
       rot-close-re01.
           if re01-stat = "A"
              close arqre01
              move "F" to re01-stat
           end-if.
      *
       rot-erro-leitura-re01.
           move " Erro de leitura - ARQRE01A.DAT - Tecle <Enter>" to
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
           read arqab01 invalid key move 1 to erro.
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
       rot-cod-n.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-doc-ncad.
           move " Documento nao cadastrado - Confirma (S) (N) ?" to
           mensagem.
           display tela-erro-cad.
           move spaces to resposta.
           perform accept-resposta-cad 
                   until resposta = "S" or "N" or escape-key = 1.
           display tela-limpa-cad.
      *
       rot-valor-n.
           move " Valor nao confere - Confirma (S) (N) ?" to
           mensagem.
           display tela-erro-cad.
           move spaces to resposta.
           perform accept-resposta-cad 
                   until resposta = "S" or "N" or escape-key = 1.
           display tela-limpa-cad.
      *
       rot-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-data-m.
           move " Data do vencimento menor que  - Tecle <Enter>" 
           to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-inic-arquivo.
           perform lmp-baixa thru lmp-liquidacao.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-baixa thru lmp-liquidacao.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
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
           display tela-limpa-cad.
      *
       rot-doc-n-conf.
           move " Documento nao confere com o codigo - Tecle <Enter>"
                to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           perform lmp-documento thru lmp-liquidacao.
           display tela-limpa-cad.
 *
       err-doc-n.
           move " Documento nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           perform lmp-documento thru lmp-liquidacao.
           display tela-limpa-cad.
      *
       rot-cod-obrig.
           move " Codigo obrigatorio - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-display.
           perform rot-move-campos-be01.
           perform rot-descricao.
           perform dsp-baixa thru dsp-liquidacao.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-display-re01.
           perform rot-move-campos-re01.
           if codigo = 0
              move re01-codigo to ab01-codigo codigo
              move re01-condicao to ab01-condicao condicao
              move 0 to erro
              perform rot-le-ab01
              if erro not = 0
                 move "Codigo nao cadastrado" to ab01-razao-social-a
              end-if
              move ab01-razao-social-a to razao-social
              perform dsp-condicao thru dsp-codigo
           end-if.
           perform dsp-documento thru dsp-operacao.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-pesq-associado.
           perform rot-close-ab01.
           move 08 to rotina-col-cod.
           move 10 to rotina-lin-cod.
           move "3" to rotina-borda-cod.
           move spaces to rotina-fundo-cod.
           move "S" to rotina-sombra-cod.
           move condicao to rotina-condicao-cod.
           call "rotab01" using param-menu campo-rotina-cod.
           cancel "rotab01".
           perform rot-open-ab01.
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
       acc-baixa.
           accept baixa at 0819 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-condicao.
           accept condicao at 0919 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo.
           accept codigo at 1019 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-documento.
           accept documento at 1119 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-vencimento.
           accept vencimento at 1219 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-valor.
           accept valor-aux at 1419 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-obs.
           accept obs at 1519 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-portador.
           accept portador at 1619 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-operacao.
           accept operacao at 1719 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-valor-pago.
           accept valor-aux at 1819 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-liquidacao.
           accept liquidacao at 1919 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-baixa.
           display baixa at 0819 with foreground-color 15 
                   background-color 01.
      *
       dsp-condicao.
           display condicao at 0919 with foreground-color 15 
                   background-color 01.
      *
       dsp-codigo.
           display codigo at 1019 with foreground-color 15 
                   background-color 01.
           display razao-social at 1025 with foreground-color 15 
                   background-color 01.
      *
       dsp-documento.
           display documento at 1119 with foreground-color 15 
                   background-color 01.
      *
       dsp-vencimento.
           display vencimento-disp at 1219 with foreground-color 15
                   background-color 01.
      *
       dsp-valor.
           display valor-disp at 1419 with foreground-color 15 
                   background-color 01.
      *
       dsp-obs.
           display obs at 1519 with foreground-color 15 
                   background-color 01.
      *
       dsp-portador.
           display portador at 1619 with foreground-color 15 
                   background-color 01.
           display dportador at 1623 with foreground-color 15 
                   background-color 01.
      *
       dsp-operacao.
           display operacao at 1719 with foreground-color 15 
                   background-color 01.
           display doperacao at 1723 with foreground-color 15 
                   background-color 01.
      *
       dsp-valor-pago.
           display valor-pago-disp at 1819 with foreground-color 15 
                   background-color 01.
      *
       dsp-liquidacao.
           display liquidacao-disp at 1919 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-baixa.
           display limpa at 0819 with foreground-color 15 
                   background-color 01.
      *
       lmp-condicao.
           display limpa at 0919 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo.
           display limpa at 1019 with foreground-color 15 
                   background-color 01.
      *
       lmp-documento.
           display limpa-10 at 1119 with foreground-color 15 
                   background-color 01.
      *
       lmp-vencimento.
           display limpa at 1219 with foreground-color 15 
                   background-color 01.
      *
       lmp-valor.
           display limpa at 1419 with foreground-color 15 
                   background-color 01.
      *
       lmp-obs.
           display limpa at 1519 with foreground-color 15 
                   background-color 01.
      *
       lmp-portador.
           display limpa at 1619 with foreground-color 15 
                   background-color 01.
      *
       lmp-operacao.
           display limpa at 1719 with foreground-color 15 
                   background-color 01.
      *
       lmp-valor-pago.
           display limpa at 1819 with foreground-color 15 
                   background-color 01.
      *
       lmp-liquidacao.
           display limpa at 1919 with foreground-color 15 
                   background-color 01.
      *
       sec-baixa section.
      *
       lab-bx-00-0.
           display tela-limpa-cad.
           perform rot-open-be01.
           if erro not = 0
              go to lab-bx-fim
           end-if.
           perform rot-open-re01.
           if erro not = 0
              go to lab-bx-fim
           end-if.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-bx-fim
           end-if.
           perform rot-open-tabl.
           if erro not = 0
              go to lab-bx-fim
           end-if.
      *
       lab-bx-00.
           if param-prioridade < 8
              perform sec-consulta
              go to lab-bx-fim
           end-if.
      *
       lab-bx-01.
           display tela-09.
           display tela-02.
           move 0 to codigo.
           move spaces to condicao.
           perform lmp-condicao.
           perform acc-condicao.
           if escape-key = 1
              go to lab-bx-fim
           end-if.
           if escape-key = 3
              perform lmp-codigo
              perform sec-consulta
              go to lab-bx-01
           end-if.
           move condicao to txt.
           perform rot-texto.
           move txt to condicao.
           if condicao not = "A" and "F" and "N" and spaces
              go to lab-bx-01
           end-if.
           if condicao = spaces
              perform lmp-condicao
              display tela-limpa-cad
              go to lab-bx-02
           end-if.
           perform dsp-condicao.
           display tela-limpa-cad.
           move 0 to rotina-codigo-cod.
      *
       lab-bx-01-0.
           display tela-05.
           move rotina-codigo-cod to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              go to lab-bx-00
           end-if.
           if escape-key = 3
              perform rot-pesq-associado
              go to lab-bx-01-0
           end-if.
           if codigo = 0 
              go to lab-bx-01-0
           end-if.
           move codigo to ab01-codigo.
           move condicao to ab01-condicao.
           move 0 to erro.
           perform rot-le-ab01.
           if erro not = 0
              perform rot-cod-n
              go to lab-bx-01-0
           end-if.
           move ab01-razao-social-a to razao-social.
           perform dsp-codigo.
           display tela-limpa-cad.
      *
       lab-bx-02.
           move spaces to documento.
           perform lmp-documento.
           perform acc-documento.
           if escape-key = 1
              perform lmp-documento
              if codigo not = 0
                 move spaces to razao-social
                 perform dsp-codigo
                 go to lab-bx-01-0
              else
                 go to lab-bx-01
              end-if
           end-if.
           if documento = spaces
              go to lab-bx-02
           end-if.
           move documento to re01-documento.
           move 0 to erro.
           perform rot-le-re01.
           if erro not = 0 
              if codigo not = 0
                 perform rot-doc-ncad
                 if resposta not = "S"
                    go to lab-bx-02
                 end-if
              else
                 perform rot-cod-obrig
                 go to lab-bx-02
              end-if
           else
              perform rot-display-re01
              move "S" to flag-re01 
              if re01-codigo  = codigo
                 go to lab-bx-07-01
              else
                 perform rot-doc-n-conf
                 display tela-limpa
                 go to lab-bx-02
           end-if.
           move "N" to flag-re01.
      *
       lab-bx-03.
           move 0 to vencimento.
           perform lmp-vencimento.
           perform acc-vencimento.
           if escape-key = 1
              perform lmp-vencimento
              go to lab-bx-02
           end-if.
           if vencimento = 0
              move "C/APRES." to vencimento-disp
              perform dsp-vencimento
              go to lab-bx-04
           end-if.
           move vencimento to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-bx-03
           end-if.
           move data-disp to vencimento-disp.
           move dias-corr to vencimento
           perform dsp-vencimento.
      *
       lab-bx-04.
           move 0 to valor-aux.
           perform lmp-valor.
           perform acc-valor.
           if escape-key = 1
              perform lmp-valor
              go to lab-bx-03
           end-if.
           move valor-aux to valor.
           if valor = 0
              go to lab-bx-04
           end-if.
           move valor to valor-disp.
           perform dsp-valor.
      *
       lab-bx-05.
           move spaces to obs.
           perform lmp-obs.
           perform acc-obs.
           if escape-key = 1
              perform lmp-obs
              go to lab-bx-04
           end-if.
           move 0 to rotina-codigo.
      *
       lab-bx-06.
           move rotina-codigo to portador.
           display tela-07.
           perform lmp-portador.
           perform acc-portador.
           if escape-key = 1
              display tela-limpa-cad
              perform lmp-portador
              go to lab-bx-05
           end-if.
           if escape-key = 3
              move 4 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-bx-06
           end-if.
           if portador = 0
              go to lab-bx-06
           end-if.
           move 04 to wtab01-tipo.
           move portador to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-portador
              go to lab-bx-06
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to dportador.
           perform dsp-portador.
           display tela-limpa-cad.
           move 0 to rotina-codigo.
      *
       lab-bx-07.
           move rotina-codigo to operacao.
           display tela-08.
           perform lmp-operacao.
           perform acc-operacao.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-operacao
              go to lab-bx-06
           end-if.
           if escape-key = 3
              move 5 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-bx-07
           end-if.
           if operacao = 0
              go to lab-bx-07
           end-if.
           move 05 to wtab01-tipo.
           move operacao to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-operacao
              go to lab-bx-07
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to doperacao.
           perform dsp-operacao.
           display tela-limpa-cad.
      *
       lab-bx-07-01.
           move valor to valor-aux.
           perform lmp-valor-pago.
           perform acc-valor-pago.
           move valor-aux to valor-pago.
           if escape-key = 1
              perform lmp-valor-pago  
              if flag-re01 = "N"
                 go to lab-bx-07
              else
                 perform lmp-documento thru lmp-valor-pago
                 display tela-limpa
                 display tela-limpa
                 go to lab-bx-02
           end-if.
           move valor-pago to valor-pago-disp.
           perform dsp-valor-pago.
           if valor-pago not = valor
              perform rot-valor-n
              if resposta not = "S"
                 go to lab-bx-07-01
              end-if
           end-if.
      *
       lab-bx-08.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to liquidacao.
           perform lmp-liquidacao.
           perform acc-liquidacao.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-liquidacao
              go to lab-bx-07-01
           end-if.
           if liquidacao = 0
              go to lab-bx-08
           end-if.
           move liquidacao to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-bx-08
           end-if.
           move data-disp to liquidacao-disp.
           move dias-corr to liquidacao
           perform dsp-liquidacao.
           move "Baixar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-bx-09.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-bx-08
           end-if.
           if resposta = "N"
              perform lmp-condicao thru lmp-liquidacao
              display tela-limpa
              go to lab-bx-01
           else
              if resposta not = "S"
                 go to lab-bx-09
              end-if
           end-if.
      *
       lab-bx-10.
           move high-values to be01-controle.
           move 0 to erro.
           perform rot-ponteiro-be01.
           if erro not = 0
              go to lab-bx-fim
           end-if.
           perform rot-le-be01-lock.
           add 1 to be01-ult-bx.
           move be01-ult-bx to baixa.
           rewrite reg-be01-1 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQBE01A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-bx-fim
           end-rewrite.
           unlock arqbe01 records.
           perform rot-move-be01.
           write reg-be01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQBE01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-bx-fim
           end-write.
           if flag-re01 = "S"
              delete arqre01 invalid key 
                     move 1 to erro
                     move " Erro de exclusao - ARQRE01A.DAT - Tecle <Ent
      -              "er> " to mensagem
                     display tela-erro
                     perform rot-keypress
                     display tela-limpa
                     go to lab-bx-fim
              end-delete
           end-if.
           perform dsp-baixa.
           display tela-06.
           perform rot-keypress.
           perform lmp-baixa thru lmp-liquidacao.
           display tela-limpa.
           go to lab-bx-01.
      *
       lab-bx-fim.
           perform rot-close-tabl.
           perform rot-close-ab01.
           perform rot-close-re01.
           perform rot-close-be01.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-limpa-cad.
           display tela-10.
      *
       lab-cns-01.
           display tela-03.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 66 or 98
                                 display tela-limpa-cad
                                 perform sec-consulta-baixa
                                 display tela-03
                            when kbd2 = 67 or 99
                                 display tela-limpa-cad
                                 perform sec-consulta-codigo
                                 display tela-03
                            when kbd2 = 76 or 108
                                 display tela-limpa-cad
                                 perform sec-consulta-liquidacao
                                 display tela-03
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           display tela-limpa.
           exit.
      *
       sec-consulta-baixa section.
      *
       lab-cns-baixa-00.
           move 0 to baixa.
           perform lmp-baixa.
           perform acc-baixa.
           if escape-key = 1
              perform lmp-baixa
              go to lab-cns-baixa-fim
           end-if.
           move low-values to be01-chave.
           move baixa to be01-baixa.
           display tela-04.
      *
       lab-cns-baixa-00-a.
           start arqbe01 key is not less be01-chave.
           go to lab-cns-baixa-03.
      *
       lab-cns-baixa-01.
           perform rot-le-anterior.
           if erro not = 0 or be01-documento = documento
              perform rot-inic-arquivo
              start arqbe01 key is not less be01-chave
              move 1 to erro
              go to lab-cns-baixa-05
           end-if.
           if be01-chave = high-values
              go to lab-cns-baixa-01
           end-if.
           go to lab-cns-baixa-04.
      *
       lab-cns-baixa-02.
           start arqbe01 key is less be01-chave.
      *
       lab-cns-baixa-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-be01
              go to lab-cns-baixa-fim
           end-if.
           if be01-chave = high-values
              perform rot-fim-arquivo
              start arqbe01 key is not less be01-chave
              move spaces to documento
              move 1 to erro
              go to lab-cns-baixa-05
           end-if.
      *
       lab-cns-baixa-04.
           perform rot-display.
      *
       lab-cns-baixa-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-baixa-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-baixa-03
                    when kbd-aux = 73
                         go to lab-cns-baixa-01
                    when kbd-aux = 71
                         move low-values to be01-chave
                         go to lab-cns-baixa-00-a
                    when kbd-aux = 79
                         move high-values to be01-chave
                         go to lab-cns-baixa-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-baixa-05
           end-if.
           perform lmp-baixa thru lmp-liquidacao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-baixa-00.
      *
       lab-cns-baixa-fim.
           move zeros to campo-kbd.
           perform lmp-baixa thru lmp-liquidacao.
           display tela-limpa.
           exit.
      *
       sec-consulta-codigo section.
      *
       lab-cns-codigo-00.
           move spaces to condicao.
           perform lmp-condicao.
           perform acc-condicao.
           if escape-key = 1
              perform lmp-condicao
              go to lab-cns-codigo-fim
           end-if.
           move condicao to txt.
           perform rot-texto.
           move txt to condicao.
           if condicao not = "A" and "F" and "N" and spaces
              go to lab-cns-codigo-00
           end-if.
           perform dsp-condicao.
           move 0 to rotina-codigo-cod.
      *
       lab-cns-codigo-00-0.
           display tela-05.
           move rotina-codigo-cod to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              display tela-limpa-cad
              go to lab-cns-codigo-00
           end-if.
           if escape-key = 3
              perform rot-pesq-associado
              go to lab-cns-codigo-00-0
           end-if.
           move low-values to be01-chave-1.
           move codigo to be01-codigo.
           move condicao to be01-condicao.
           display tela-04.
      *
       lab-cns-codigo-00-a.
           start arqbe01 key is not less be01-chave-1.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or be01-baixa = baixa
              perform rot-inic-arquivo
              start arqbe01 key is not less be01-chave-1
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if be01-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqbe01 key is less be01-chave-1.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-be01
              go to lab-cns-codigo-fim
           end-if.
           if be01-chave = high-values
              perform rot-fim-arquivo
              start arqbe01 key is not less be01-chave-1
              move 0 to baixa
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
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to be01-chave-1
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to be01-chave-1
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-baixa thru lmp-liquidacao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-baixa thru lmp-liquidacao.
           display tela-limpa.
           exit.
      *
       sec-consulta-liquidacao section.
      *
       lab-cns-liquidacao-00.
           move 0 to liquidacao.
           perform lmp-liquidacao.
           perform acc-liquidacao.
           if escape-key = 1
              perform lmp-liquidacao
              go to lab-cns-liquidacao-fim
           end-if.
           if liquidacao not = 0
              move liquidacao to data-aux
              move dia-aux to dia-euro
              move mes-aux to mes-euro
              move ano-aux to ano-euro
              move 4 to opcao-data
              perform rot-data
              if return-code not = 0
                 perform rot-data-i
                 go to lab-cns-liquidacao-00
              end-if
              move dias-corr to liquidacao
           end-if.
           move low-values to be01-chave-2.
           move liquidacao to be01-liquidacao.
           display tela-04.
      *
       lab-cns-liquidacao-00-a.
           start arqbe01 key is not less be01-chave-2.
           go to lab-cns-liquidacao-03.
      *
       lab-cns-liquidacao-01.
           perform rot-le-anterior.
           if erro not = 0 or be01-baixa = baixa
              perform rot-inic-arquivo
              start arqbe01 key is not less be01-chave-2
              move 1 to erro
              go to lab-cns-liquidacao-05
           end-if.
           if be01-chave = high-values
              go to lab-cns-liquidacao-01
           end-if.
           go to lab-cns-liquidacao-04.
      *
       lab-cns-liquidacao-02.
           start arqbe01 key is less be01-chave-2.
      *
       lab-cns-liquidacao-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-be01
              go to lab-cns-liquidacao-fim
           end-if.
           if be01-chave = high-values
              perform rot-fim-arquivo
              start arqbe01 key is not less be01-chave-2
              move 0 to baixa
              move 1 to erro
              go to lab-cns-liquidacao-05
           end-if.
      *
       lab-cns-liquidacao-04.
           perform rot-display.
      *
       lab-cns-liquidacao-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-liquidacao-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-liquidacao-03
                    when kbd-aux = 73
                         go to lab-cns-liquidacao-01
                    when kbd-aux = 71
                         move low-values to be01-chave-2
                         go to lab-cns-liquidacao-00-a
                    when kbd-aux = 79
                         move high-values to be01-chave-2
                         go to lab-cns-liquidacao-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-liquidacao-05
           end-if.
           perform lmp-baixa thru lmp-liquidacao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-liquidacao-00.
      *
       lab-cns-liquidacao-fim.
           move zeros to campo-kbd.
           perform lmp-baixa thru lmp-liquidacao.
           display tela-limpa.
           exit.
      *
       sec-alteracao section.
      *
       lab-alt-00-0.
           display tela-limpa-cad.
           if param-prioridade < 8
              perform display-erro-usr
              go to lab-alt-fim
           end-if.
           perform rot-ponteiro-be01.
           if erro not = 0
              go to lab-alt-fim
           end-if.
           perform rot-le-be01-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform acc-obs.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move portador to rotina-codigo.
      *
       lab-alt-02.
           move rotina-codigo to portador.
           display tela-07.
           perform lmp-portador.
           perform acc-portador.
           if escape-key = 1
              display tela-limpa-cad
              perform dsp-portador
              go to lab-alt-01
           end-if.
           if escape-key = 3
              move 4 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-02
           end-if.
           if portador = 0
              go to lab-alt-02
           end-if.
           move 04 to wtab01-tipo.
           move portador to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-portador
              go to lab-alt-02
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to dportador.
           perform dsp-portador.
           display tela-limpa-cad.
           move operacao to rotina-codigo.
      *
       lab-alt-03.
           move rotina-codigo to operacao.
           display tela-08.
           perform lmp-operacao.
           perform acc-operacao.
           if escape-key = 1
              perform dsp-operacao
              go to lab-alt-02
           end-if.
           if escape-key = 3
              move 5 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-03
           end-if.
           if operacao = 0
              go to lab-alt-03
           end-if.
           move 05 to wtab01-tipo.
           move operacao to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-operacao
              go to lab-alt-03
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to doperacao.
           perform dsp-operacao.
           display tela-limpa-cad.
      *
       lab-alt-04.
           perform lmp-valor-pago.
           perform acc-valor-pago.
           if escape-key = 1
              move valor-pago to valor-pago-disp
              perform dsp-valor-pago  
              go to lab-alt-03
           end-if.
           move valor-aux to valor-pago valor-pago-disp.
           perform dsp-valor-pago.
           if valor-pago not = be01-valor-pago
              perform rot-valor-n
              if resposta not = "S"
                 go to lab-alt-04
              end-if
           end-if.
      *
       lab-alt-05.
           perform lmp-liquidacao.
           perform acc-liquidacao.
           if escape-key = 1
              perform dsp-liquidacao
              go to lab-alt-04
           end-if.
           if liquidacao = 0
              go to lab-alt-05
           end-if.
           move liquidacao to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              move be01-liquidacao to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to liquidacao
              move data-disp to liquidacao-disp
              go to lab-alt-05
           end-if.
           move data-disp to liquidacao-disp.
           move dias-corr to liquidacao
           perform dsp-liquidacao.
      *
       lab-alt-06.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              move liquidacao to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to liquidacao
              go to lab-alt-05
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-06
              end-if
           end-if.
           perform rot-move-be01.
           rewrite reg-be01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQBE01A.DAT - Tecle <Ent
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
           unlock arqbe01 record.
           display tela-04.
           exit.