      ***************************************************************
      *                                                             *
      *  E P S  - I N F O R M A T I C A    :::  PGAB0A              *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao de TKT's :                                      *
      *                                                             *
      *  Data da ultima alteracao:    08/12/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab0a.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqab02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab02-chave
                  alternate record key is ab02-chave-1 with duplicates
                  file status is ab02-status.
      *
           select arqab05 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab05-chave
                  alternate record key is ab05-chave-1 with duplicates
                  alternate record key is ab05-chave-2 with duplicates
                  alternate record key is ab05-chave-3 with duplicates
                  alternate record key is ab05-chave-4 with duplicates
                  alternate record key is ab05-chave-5 with duplicates
                  alternate record key is ab05-chave-6 with duplicates
                  alternate record key is ab05-chave-7 with duplicates
                  alternate record key is ab05-chave-8 with duplicates
                  alternate record key is ab05-chave-9 with duplicates
                  alternate record key is ab05-chave-10 with duplicates
                  file status is ab05-status.
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
            select arqusr assign to disk
                   organization is indexed
                   access mode is dynamic
                   lock mode is manual
                   with lock on record
                   record key is usr-chave
                   file status is usr-status.
      *
       data division.
       file section.
      *    
       copy fdab02.lib.
      *    
       copy fdab05.lib.
      *
       copy fdtabl.lib.
      *
       copy fdusr.lib.
      *    
       working-storage section.
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
       01 ab05-status                  pic x(02) value "00".
       01 ab05-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab05.
          02 ab05-dir                  pic x(03) value "AB3".
          02 filler                    pic x(01) value "\".
          02 ab05-nome                 pic x(08) value "ARQAB05A".
          02 filler                    pic x(01) value ".".
          02 ab05-ext                  pic x(03) value "DAT".
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
       01 usr-status                  pic x(02) value "00".
       01 usr-stat                    pic x(01) value "F".
      *
       01 nome-arq-usr.
          02 usr-dir                   pic x(03) value "USR".
          02 filler                    pic x(01) value "\".
          02 usr-nome                  pic x(08) value "ARQUSR00".
          02 filler                    pic x(01) value ".".
          02 usr-ext                   pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAB0A".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(50) value spaces.
       01 limpa-03                     pic x(03) value spaces.
       01 limpa-10                     pic x(10) value spaces.
       01 limpa-22                     pic x(22) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 codigo                    pic 9(05) value 0.
          02 cons                      pic 9(05) value 0.
          02 dcons                     pic x(40) value spaces.
          02 data-rec                  pic 9(06) value 0.
          02 data-rec-disp             pic x(08) value spaces.
          02 pax                       pic x(30) value spaces.
          02 pax-a                     pic x(30) value spaces.
          02 empresa                   pic x(20) value spaces.
          02 empresa-a                 pic x(20) value spaces.
          02 destino                   pic x(03) value spaces.
          02 data-emb                  pic 9(06) value 0.
          02 data-emb-disp             pic x(08) value spaces.
          02 horario-e                 pic 9(04) value 0.
          02 horario-e-disp            pic 99b99 value 0.
          02 voo                       pic 9(04) value 0.
          02 cia                       pic x(02) value spaces.
          02 dcia                      pic x(17) value spaces.
          02 blc                       pic 9(01) value 0.
          02 data-ent                  pic 9(06) value 0.
          02 data-ent-disp             pic x(08) value spaces.
          02 horario                   pic 9(04) value 0.
          02 horario-disp              pic 99b99 value 0.
          02 atendente                 pic x(10) value spaces.
          02 obs                       pic x(40) occurs 2.
          02 aerop                     pic 9(01) value 0.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 horario-r.
          02 hora-r                    pic 9(02) value 0.
          02 min-r                     pic 9(02) value 0.
      *
       01 campo-rotina-ab02.
          02 rotina-ab02-codigo        pic 9(05) value 0.
          02 rotina-ab02-nivel         pic 9(01) value 0.
      *
       01 campo-rotina-cia.
          02 rotina-col-cia            pic 9(02) value 0.
          02 rotina-lin-cia            pic 9(02) value 0.
          02 rotina-borda-cia          pic x(01) value spaces.
          02 rotina-fundo-cia          pic x(01) value spaces.
          02 rotina-sombra-cia         pic x(01) value spaces.
          02 rotina-tipo-cia           pic 9(02) value 0.
          02 rotina-cia                pic x(02) value spaces.
      *
       01 campo-rotina-cid.
          02 rotina-col-cid            pic 9(02) value 0.
          02 rotina-lin-cid            pic 9(02) value 0.
          02 rotina-borda-cid          pic x(01) value spaces.
          02 rotina-fundo-cid          pic x(01) value spaces.
          02 rotina-sombra-cid         pic x(01) value spaces.
          02 rotina-tipo-cid           pic 9(02) value 0.
          02 rotina-cid                pic x(03) value spaces.
      *
       01 campo-rotina-usr.
          02 rotina-col-usr            pic 9(02) value 0.
          02 rotina-lin-usr            pic 9(02) value 0.
          02 rotina-borda-usr          pic x(01) value spaces.
          02 rotina-fundo-usr          pic x(01) value spaces.
          02 rotina-sombra-usr         pic x(01) value spaces.
          02 rotina-usr                pic x(10) value spaces.
      *
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
      *
       01 campo-string.
          02 string-a                  pic x(40) value spaces.
          02 filler                    pic x(01) value low-values.
      *
       01 campo-lixo.
         02 lixo                       pic x(1024) value spaces.
         02 filler                     pic x(01) value low-values. 
      *
       01 buffer1.
          02 filler                    pic 9(04) occurs 500.
      *
       copy wstab04.lib.
       copy wstab05.lib.
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
          02 line 07 column 06 foreground-color 06 background-color 01
             highlight value "Codigo....:".
          02 line 08 column 06 foreground-color 06 background-color 01
             highlight value "Agencia...:".
          02 line 09 column 06 foreground-color 06 background-color 01
             highlight value "Data Receb:".
          02 line 10 column 06 foreground-color 06 background-color 01
             highlight value "Pax.......:".
          02 line 11 column 06 foreground-color 06 background-color 01
             highlight value "Empresa...:".
          02 line 11 column 40 foreground-color 06 background-color 01
             highlight value "Destino...:".
          02 line 12 column 06 foreground-color 06 background-color 01
             highlight value "Embarque..:".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "H. Emb....:".
          02 line 13 column 40 foreground-color 06 background-color 01
             highlight value "Voo.......:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Cia.Aerea.:".
          02 line 14 column 40 foreground-color 06 background-color 01
             highlight value "Aeroporto.:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Entrega...:".
          02 line 16 column 40 foreground-color 06 background-color 01
             highlight value "H. Ent....:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Atendente.:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Observacao:".
      *
       01 tela-02.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 21 column 15 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 17 foreground-color 05 background-color 03
             value "-Consultas".
          02 line 21 column 31 foreground-color 02 background-color 03
             highlight value "F3".
          02 line 21 column 33 foreground-color 05 background-color 03
             value "-Consorciadas".
      *
       01 tela-03.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 21 column 16 foreground-color 02 background-color 03
             highlight value "C".
          02 line 21 column 17 foreground-color 05 background-color 03
             value "odigo".
          02 line 21 column 25 foreground-color 02 background-color 03
             highlight value "A".
          02 line 21 column 26 foreground-color 05 background-color 03
             value "gencia".
         02 line 21 column 36 foreground-color 02 background-color 03
             highlight value "P".
          02 line 21 column 37 foreground-color 05 background-color 03
             value "ax".
         02 line 21 column 42 foreground-color 02 background-color 03
             highlight value "E".
          02 line 21 column 43 foreground-color 05 background-color 03
             value "mpresa".
         02 line 21 column 52 foreground-color 02 background-color 03
             highlight value "D".
          02 line 21 column 53 foreground-color 05 background-color 03
             value "estino".
          02 line 21 column 62 foreground-color 05 background-color 03
             value "E barque".
         02 line 21 column 63 foreground-color 02 background-color 03
             highlight value "m".
      *
       01 tela-04.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Alt".
          02 line 21 column 14 foreground-color 02 background-color 03 
             highlight value "F3".
          02 line 21 column 16 foreground-color 05 background-color 03
             value "-Exc".
          02 line 21 column 21 foreground-color 02 background-color 03 
             highlight value "F4".
          02 line 21 column 23 foreground-color 05 background-color 03
             value "-Obs".
          02 line 21 column 29 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 21 column 33 foreground-color 05 background-color 03
             value "-Inic".
          02 line 21 column 40 foreground-color 02 background-color 03
             highlight value "End".
          02 line 21 column 43 foreground-color 05 background-color 03
             value "-Fim".
          02 line 21 column 50 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 21 column 56 foreground-color 05 background-color 03
             value "-Prox".
          02 line 21 column 63 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 21 column 67 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-06.
          02 line 21 column 05 foreground-color 00 background-color 01
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 07 background-color 01
             highlight value "Registro gravado Tecle <Enter>".
      *
       01 tela-08.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Cidades".
      *
       01 tela-09.
          02 line 06 column 65 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 06 column 65 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-12.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Cias. Aereas".
      *
       01 tela-14.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Atendentes".
      *
       01 tela-15.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Consorciadas".
      *
       01 tela-16.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 21 column 10 foreground-color 05 background-color 03
             value "- CGH".
          02 line 21 column 18 foreground-color 02 background-color 03
             highlight value "2".
          02 line 21 column 20 foreground-color 05 background-color 03
             value "- GRU".
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
           move 04 to box-lin.
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
       rot-move-AB05.
           move codigo to ab05-codigo.
           move cons to ab05-cons.
           move data-rec to ab05-data-rec.
           move pax to ab05-pax.
           move pax-a to ab05-pax-a.
           move empresa to ab05-empresa.
           move empresa-a to ab05-empresa-a.
           move destino to ab05-destino.
           move data-emb to ab05-data-emb.
           move horario-e to ab05-horario-e.
           move cia to ab05-cia.
           move voo to ab05-voo.
           move aerop to ab05-aerop.
           move data-ent to ab05-data-ent.
           move horario to ab05-horario.
           move atendente to ab05-atendente.
           move obs (01) to ab05-obs (01).
           move obs (02) to ab05-obs (02).
           move param-usr to ab05-usuario.
           move param-data to ab05-data.
      *
       rot-move-campos.
           move ab05-codigo to codigo.
           move ab05-cons to cons.
           move ab05-data-rec to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to data-rec-disp.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to data-rec.
           move ab05-pax-a to pax pax-a.
           move ab05-empresa-a to empresa empresa-a.
           move ab05-destino to destino.
           if ab05-data-emb not = 0
              move ab05-data-emb to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to data-emb-disp
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-emb
           else
              move spaces to data-emb-disp
              move 0 to data-emb
           end-if.
           move ab05-horario-e to horario-e horario-e-disp.
           move ab05-cia to cia.
           move ab05-voo to voo.
           move ab05-aerop to aerop.
           if ab05-data-ent not = 0
              move ab05-data-ent to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to data-ent-disp
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-ent
              move ab05-horario to horario horario-disp
           else
              move 0 to data-ent horario
              move spaces to data-ent-disp 
              move 9999 to horario-disp
           end-if.
           move ab05-atendente to atendente.
           move ab05-obs (01) to obs (01).
           move ab05-obs (02) to obs (02).
           move ab05-data to dias-corr.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move ab05-usuario to cab-usuario.
      *
       rot-le-ab02.
           move 0 to erro.
           read arqab02 invalid key move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab02.
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
       rot-erro-leitura-ab02.
           move " Erro de leitura - ARQAB02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqab05 key is equal ab05-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-ab05
           end-start.
      *
       rot-le-ab05-lock.
           move 0 to erro.
           read arqab05 next. 
           if ab05-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ab05-lock
           end-if.
           read arqab05 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqab05 previous at end move 1 to erro.
           if ab05-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqab05 next at end move 1 to erro.
           if ab05-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ab05.
           move 0 to erro.
           if ab05-stat = "F"
              open i-o arqab05
              if ab05-status not = "00"
                 move 
                 " Erro de abertura no ARQAB05A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
                 move zeros to reg-ab05
                 move high-values to ab05-controle
                 move 0 to ab05-numero
                 write reg-ab05-1
               else
                  move "A" to ab05-stat
               end-if
           end-if.
      *
       rot-close-ab05.
           if ab05-stat = "A"
              close arqab05
              move "F" to ab05-stat
           end-if.
      *
       rot-erro-leitura-ab05.
           move " Erro de leitura - ARQAB05A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-obs-2.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-obs-2.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
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
       rot-open-usr.
           move 0 to erro.
           if usr-stat = "F"
              open i-o arqusr
              if usr-status not = "00"
                 move 
                 " Erro de abertura no ARQUSR00.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
              else
                 move "A" to usr-stat
              end-if
           end-if.
      *
       rot-close-usr.
           if usr-stat = "A"
              close arqusr
              move "F" to usr-stat
           end-if.
      *
       rot-le-usr.
           move 0 to erro.
           read arqusr invalid key move 1 to erro.
           if usr-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-usr.
      *
       rot-display.
           perform rot-move-campos.
           move cons to ab02-codigo.
           perform rot-le-ab02.
           if erro not = 0
              move "Consorciada nao cadastrada" to dcons
           else
              move ab02-razao-social-a to dcons
           end-if.
           move 09 to wtab05-tipo.
           move cia to wtab05-sigla.
           move spaces to wtab05-resto.
           move wtab05-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              move "Cia. aerea nao cadastrada" to dcia
           else
              move reg-tabl to reg-wtab05
              move wtab05-descricao to dcia
           end-if.
           perform dsp-codigo thru dsp-obs-2.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       pesq-cons.
           move 1 to rotina-ab02-nivel.
           call "pgab02" using param-menu campo-rotina-ab02.
           cancel "pgab02".
      *
       rot-pesq-cid.
           move 15 to rotina-col-cid.
           move 11 to rotina-lin-cid.
           move "3" to rotina-borda-cid.
           move spaces to rotina-fundo-cid.
           move "S" to rotina-sombra-cid.
           move 8 to rotina-tipo-cid.
           call "pgtab03" using param-menu 
                                campo-rotina-cid.
          cancel "pgtab03".
      *
       rot-pesq-cia.
           move 15 to rotina-col-cia.
           move 11 to rotina-lin-cia.
           move "3" to rotina-borda-cia.
           move spaces to rotina-fundo-cia.
           move "S" to rotina-sombra-cia.
           move 9 to rotina-tipo-cia.
           call "pgtab04" using param-menu 
                                campo-rotina-cia.
          cancel "pgtab04".
      *
       rot-pesq-usr.
           move 15 to rotina-col-usr.
           move 11 to rotina-lin-usr.
           move "3" to rotina-borda-usr.
           move spaces to rotina-fundo-usr.
           move "S" to rotina-sombra-usr.
           call "pgusr01" using param-menu 
                                campo-rotina-usr.
          cancel "pgusr01".
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
       err-codigo-n.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-tkt-c.
           move " TKT ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-cidade-n.
           move " Cidade nao cadastrada - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-cia-n.
           move " Cia. Aerea nao cadastrada - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-atendente-n.
           move " Atendente nao cadastrado - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-data-emb.
           move " Data embarque menor que recebimento - Tecle <Enter>"
           to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-data-ent.
           move " Data entrega menor que recebimento - Tecle <Enter>"
           to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-rateio.
           move " Status do rateio = 'N' - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
      *  Sequencia para dar Accept
      *
       acc-codigo.
           accept codigo at 0718 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cons.
           accept cons at 0818 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-rec.
           accept data-rec at 0918 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-pax.
           accept pax at 1018 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-empresa.
           accept empresa at 1118 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-destino.
           accept destino at 1152 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-emb.
           accept data-emb at 1218 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-horario-e.
           accept horario-e at 1318 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
      *
       acc-voo.
           accept voo at 1352 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cia.
           accept cia at 1418 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-aerop.
           accept aerop at 1452 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-ent.
           accept data-ent at 1618 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-horario.
           accept horario at 1652 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-atendente.
           accept atendente at 1718 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-obs-1.
           accept obs (01) at 1818 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-obs-2.
           accept obs (02) at 1918 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           display codigo at 0718 with foreground-color 15 
                   background-color 01.
      *
       dsp-cons.
           display cons at 0818 with foreground-color 15 
                   background-color 01.
           display dcons at 0824 with foreground-color 15 
                   background-color 01.
      *
       dsp-data-rec.
           display data-rec-disp at 0918 with foreground-color 15 
                   background-color 01.
      *
       dsp-pax.
           display pax at 1018 with foreground-color 15 
                   background-color 01.
      *
       dsp-empresa.
           display empresa at 1118 with foreground-color 15 
                   background-color 01.
      *
       dsp-destino.
           display destino at 1152 with foreground-color 15 
                   background-color 01.
      *
       dsp-data-emb.
           display data-emb-disp at 1218 with foreground-color 15 
                   background-color 01.
      *
       dsp-horario-e.
           inspect horario-e-disp replacing all " " by ":".
           display horario-e-disp at 1318 with foreground-color 15 
                   background-color 01.
      *
       dsp-voo.
           display voo at 1352 with foreground-color 15 
                   background-color 01.
      *
       dsp-cia.
           display cia at 1418 with foreground-color 15 
                   background-color 01.
           display dcia at 1422 with foreground-color 15 
                   background-color 01.
      *
       dsp-aerop.
           display aerop at 1452 with foreground-color 15 
                   background-color 01.
      *
       dsp-data-ent.
           display data-ent-disp at 1618 with foreground-color 15 
                   background-color 01.
      *
       dsp-horario.
           if horario-disp not = "99 99"
              inspect horario-disp replacing all " " by ":"
              display horario-disp at 1652 with foreground-color 15 
                      background-color 01
           else
              perform lmp-horario
           end-if.

      *
       dsp-atendente.
           display atendente at 1718 with foreground-color 15 
                   background-color 01.
      *
       dsp-obs-1.
           display obs (01) at 1818 with foreground-color 15 
                   background-color 01.
      *
       dsp-obs-2.
           display obs (02) at 1918 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-codigo.
           display limpa at 0718 with foreground-color 15 
                   background-color 01.
      *
       lmp-cons.
           display limpa at 0818 with foreground-color 15 
                   background-color 01.
      *
       lmp-data-rec.
           display limpa-10 at 0918 with foreground-color 15 
                   background-color 01.
      *
       lmp-pax.
           display limpa at 1018 with foreground-color 15 
                   background-color 01.
      *
       lmp-empresa.
           display limpa-22 at 1118 with foreground-color 15 
                   background-color 01.
      *
       lmp-destino.
           display limpa-10 at 1152 with foreground-color 15 
                   background-color 01.
      *
       lmp-data-emb.
           display limpa-10 at 1218 with foreground-color 15 
                   background-color 01.
      *
       lmp-horario-e.
           display limpa-10 at 1318 with foreground-color 15 
                   background-color 01.
      *
       lmp-voo.
           display limpa-10 at 1352 with foreground-color 15 
                   background-color 01.
      *
       lmp-cia.
           display limpa-22 at 1418 with foreground-color 15 
                   background-color 01.
      *
       lmp-aerop.
           display limpa-10 at 1452 with foreground-color 15 
                   background-color 01.
      *
       lmp-data-ent.
           display limpa-10 at 1618 with foreground-color 15 
                   background-color 01.
      *
       lmp-horario.
           display limpa-10 at 1652 with foreground-color 15 
                   background-color 01.
      *
       lmp-atendente.
           display limpa at 1718 with foreground-color 15 
                   background-color 01.
      *
       lmp-obs-1.
           display limpa at 1818 with foreground-color 15 
                   background-color 01.
      *
       lmp-obs-2.
           display limpa at 1918 with foreground-color 15 
                   background-color 01.
      *
       sec-inclusao section.
      *
           display tela-limpa-cad.
           perform rot-open-tabl
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-ab02.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-ab05.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-usr.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           display tela-09.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
      *
       lab-inc-01-00.
           display tela-09.
           initialize campos.
           perform lmp-codigo thru lmp-obs-2.
      *
       lab-inc-01.
           display tela-02.
           perform lmp-cons.
           perform acc-cons.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-codigo thru lmp-obs-2
              perform sec-consulta
              go to lab-inc-01-00
           end-if.
           if escape-key = 4
              perform pesq-cons
              move rotina-ab02-codigo to cons
              go to lab-inc-01
           end-if.
           if cons = 0
              go to lab-inc-01
           end-if.
           move cons to ab02-codigo.
           perform rot-le-ab02.
           if erro not = 0
              perform err-codigo-n
              go to lab-inc-01
           end-if.
           move ab02-razao-social-a to dcons.
           perform dsp-cons.
           if ab02-rateio = "N"
              perform err-rateio
              go to lab-inc-01
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02.
           perform lmp-data-rec.
           perform acc-data-rec.
           if escape-key = 1
              go to lab-inc-01
           end-if.
           if data-rec = 0
              go to lab-inc-02
           end-if.
           move data-rec to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-inc-02
           end-if.
           move data-disp to data-rec-disp.
           move dias-corr to data-rec
           perform dsp-data-rec.
      *
       lab-inc-03.
           perform acc-pax.
           if escape-key = 1
              move data-rec to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-rec
              go to lab-inc-02
           end-if.
           move pax to txt pax-a.
           perform rot-texto.
           if txt = spaces 
              go to lab-inc-03
           end-if.
           move txt to pax.
      *
       lab-inc-04.
           perform acc-empresa.
           if escape-key = 1
              move pax-a to pax
              go to lab-inc-03
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              move spaces to empresa empresa-a
              perform dsp-empresa
           else
              move txt to empresa
           end-if.
      *
       lab-inc-05.
           display tela-08.
           perform acc-destino.
           if escape-key = 1
              move empresa-a to empresa
              display tela-limpa-cad
              go to lab-inc-04
           end-if.
           if escape-key = 3
              perform rot-pesq-cid
              move rotina-cid to destino
              go to lab-inc-05
           end-if.
           move destino to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-05
           end-if.
           move txt to destino.
           perform dsp-destino.
           move 08 to wtab04-tipo.
           move destino to wtab04-sigla.
           move spaces to wtab04-resto.
           move wtab04-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-cidade-n
              go to lab-inc-05
           end-if.
           move reg-tabl to reg-wtab04.
           display tela-limpa-cad.
      *
       lab-inc-06.
           perform lmp-data-emb.
           perform acc-data-emb.
           if escape-key = 1
              go to lab-inc-05
           end-if.
           if data-emb = 0
              perform lmp-data-emb
              go to lab-inc-07
           end-if.
           move data-emb to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-inc-06
           end-if.
           if dias-corr < data-rec
              perform err-data-emb
              go to lab-inc-06
           end-if.
           move data-disp to data-emb-disp.
           move dias-corr to data-emb
           perform dsp-data-emb.
      *
       lab-inc-07.
           perform lmp-horario-e.
           perform acc-horario-e.
           if escape-key = 1
              if data-emb not = 0
                 move data-emb to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to data-emb
              end-if
              go to lab-inc-06
           end-if.
           move horario-e to horario-r horario-e-disp.
           if hora-r > 23 or min-r > 59
              go to lab-inc-07
           end-if.
           perform dsp-horario-e.
      *
       lab-inc-08.
           perform acc-voo.
           if escape-key = 1 
              go to lab-inc-07
           end-if.
      *
       lab-inc-09.
           display tela-12
           perform lmp-cia.
           perform acc-cia.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-08
           end-if.
           if escape-key = 3
              perform rot-pesq-cia
              move rotina-cia to cia
              go to lab-inc-09
           end-if.
           move cia to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-09
           end-if.
           move txt to cia.
           move spaces to dcia.
           perform dsp-cia.
           move 09 to wtab05-tipo.
           move cia to wtab05-sigla.
           move spaces to wtab05-resto.
           move wtab05-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-cia-n
              go to lab-inc-09
           end-if.
           move reg-tabl to reg-wtab05.
           move wtab05-descricao to dcia.
           perform dsp-cia.
           display tela-limpa-cad.
      *
       lab-inc-10.
           display tela-16.
           perform acc-aerop.
           if escape-key = 1
              go to lab-inc-09
           end-if.
           if aerop not = 1 and 2
              go to lab-inc-10
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-11.
           perform lmp-data-ent.
           perform acc-data-ent.
           if escape-key = 1
              go to lab-inc-10
           end-if.
           if data-ent = 0
              perform lmp-data-ent
              go to lab-inc-14
           end-if.
           move data-ent to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-inc-11
           end-if.
           if dias-corr < data-rec
              perform err-data-ent
              go to lab-inc-11
           end-if.
           move data-disp to data-ent-disp.
           move dias-corr to data-ent
           perform dsp-data-ent.
      *
       lab-inc-12.
           perform lmp-horario.
           perform acc-horario.
           if escape-key = 1
              move data-ent to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-ent
              go to lab-inc-11
           end-if.
           move horario to horario-r horario-disp.
           if hora-r > 23 or min-r > 59
              go to lab-inc-12
           end-if.
           perform dsp-horario.
      *
       lab-inc-13.
           display tela-14.
           perform acc-atendente.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-12
           end-if.
           if escape-key = 3
              perform rot-pesq-usr
              move rotina-usr to atendente
              go to lab-inc-13
           end-if.
           move atendente to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-13
           end-if.
           move txt to atendente usr-usuario.
           perform dsp-atendente.
           perform rot-le-usr.
           if erro not = 0
              perform err-atendente-n
              go to lab-inc-13
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-14.
           perform acc-obs-1.
           if escape-key = 1
              if data-ent = 0
                 go to lab-inc-11
              else
                 go to lab-inc-13
             end-if
           end-if.
           if obs (01) = spaces
              go to lab-inc-16
           end-if.
      *
       lab-inc-15.
           perform acc-obs-2.
           if escape-key = 1
              go to lab-inc-14
           end-if.
      *
       lab-inc-16.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              if obs (01) = spaces
                 go to lab-inc-14
              else
                 go to lab-inc-15
              end-if
           end-if.
           if resposta = "N"
              move pax-a to pax
              move empresa-a to empresa
              move data-rec to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-rec
              if data-emb not = 0
                 move data-emb to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to data-emb
              end-if
              if data-ent not = 0
                 move data-ent to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to data-ent
              end-if
              display tela-limpa-cad
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-12
              end-if
           end-if.
      *
       lab-inc-17.
           move high-values to ab05-chave-controle.
           perform rot-ponteiro.
           perform rot-le-ab05-lock.
           if erro not = 0
              move " Erro no registro de controle - ARQAB05A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-inc-fim
           end-if.
           move ab05-numero to codigo.
           add 1 to ab05-numero codigo.
           rewrite reg-ab05.
           unlock arqab05 record.
           perform rot-move-ab05.
           write reg-ab05 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAB05A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           perform dsp-codigo.
           display tela-06.
           perform rot-keypress.
           perform lmp-codigo.
           perform lmp-data-rec thru lmp-obs-2.
           initialize campos.
           move ab05-cons to cons.
           display tela-02.
           go to lab-inc-02.
      *
       lab-inc-fim.
           perform rot-close-usr.
           perform rot-close-ab05.
           perform rot-close-ab02.
           perform rot-close-tabl.
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
                            when kbd2 = 67 or 99
                                 display tela-limpa-cad
                                 perform sec-consulta-codigo
                                 display tela-03
                            when kbd2 = 65 or 97
                                 display tela-limpa-cad
                                 move 0 to cons
                                 perform sec-consulta-cons
                                 display tela-03
                            when kbd2 = 80 or 112
                                 display tela-limpa-cad
                                 perform sec-consulta-pax
                                 display tela-03
                            when kbd2 = 69 or 101
                                 display tela-limpa-cad
                                 perform sec-consulta-empresa
                                 display tela-03
                            when kbd2 = 68 or 100
                                 display tela-limpa-cad
                                 perform sec-consulta-destino
                                 display tela-03
                            when kbd2 = 77 or 109
                                 display tela-limpa-cad
                                 perform sec-consulta-embarque
                                 display tela-03
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           display tela-limpa.
           exit.
      *
       sec-consulta-codigo section.
      *
       lab-cns-codigo-00.
           move 0 to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              go to lab-cns-codigo-fim
           end-if.
      *
           display tela-04.
           move low-values to ab05-chave.
           move codigo to ab05-codigo.
      *
       lab-cns-codigo-00-a.
           start arqab05 key is not less ab05-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or ab05-codigo = codigo
              perform rot-inic-arquivo
              start arqab05 key is not less ab05-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if ab05-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqab05 key is less ab05-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab05
              go to lab-cns-codigo-fim
           end-if.
           if ab05-chave = high-values
              perform rot-fim-arquivo
              start arqab05 key is not less ab05-chave
              move 0 to codigo
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
                         move low-values to ab05-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to ab05-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa.
           exit.
      *
       sec-consulta-cons section.
      *
       lab-cns-cons-00.
           display tela-15.
           perform lmp-cons.
           perform acc-cons.
           if escape-key = 1
              perform lmp-cons
              go to lab-cns-cons-fim
           end-if.
           if escape-key = 3
              perform pesq-cons
              move rotina-ab02-codigo to cons
              go to lab-cns-cons-00
           end-if.
      *
           display tela-04.
           move low-values to ab05-chave-1.
           move codigo to ab05-cons.
      *
       lab-cns-cons-00-a.
           start arqab05 key is not less ab05-chave-1.
           go to lab-cns-cons-03.
      *
       lab-cns-cons-01.
           perform rot-le-anterior.
           if erro not = 0 or ab05-codigo = codigo
              perform rot-inic-arquivo
              start arqab05 key is not less ab05-chave-1
              move 1 to erro
              go to lab-cns-cons-05
           end-if.
           if ab05-chave = high-values
              go to lab-cns-cons-01
           end-if.
           go to lab-cns-cons-04.
      *
       lab-cns-cons-02.
           start arqab05 key is less ab05-chave-1.
      *
       lab-cns-cons-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab05
              go to lab-cns-cons-fim
           end-if.
           if ab05-chave = high-values
              perform rot-fim-arquivo
              start arqab05 key is not less ab05-chave-1
              move 0 to codigo
              move 1 to erro
              go to lab-cns-cons-05
           end-if.
      *
       lab-cns-cons-04.
           perform rot-display.
      *
       lab-cns-cons-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-cons-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-cons-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-cons-03
                    when kbd-aux = 73
                         go to lab-cns-cons-01
                    when kbd-aux = 71
                         move low-values to ab05-chave-1
                         go to lab-cns-cons-00-a
                    when kbd-aux = 79
                         move high-values to ab05-chave-1
                         go to lab-cns-cons-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-cons-05
           end-if.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa-cad.
           display tela-limpa.
           move 0 to cons.
           go to lab-cns-cons-00.
      *
       lab-cns-cons-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa.
           exit.
      *
       sec-consulta-pax section.
      *
       lab-cns-pax-00.
           move spaces to pax.
           perform lmp-pax.
           perform acc-pax.
           if escape-key = 1
              perform lmp-cons
              go to lab-cns-pax-fim
           end-if.
           display tela-04.
           move pax to txt.
           perform rot-texto.
           move low-values to ab05-chave-3.
           move txt to ab05-pax.
      *
       lab-cns-pax-00-a.
           start arqab05 key is not less ab05-chave-3.
           go to lab-cns-pax-03.
      *
       lab-cns-pax-01.
           perform rot-le-anterior.
           if erro not = 0 or ab05-codigo = codigo
              perform rot-inic-arquivo
              start arqab05 key is not less ab05-chave-3
              move 1 to erro
              go to lab-cns-pax-05
           end-if.
           if ab05-chave = high-values
              go to lab-cns-pax-01
           end-if.
           go to lab-cns-pax-04.
      *
       lab-cns-pax-02.
           start arqab05 key is less ab05-chave-3.
      *
       lab-cns-pax-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab05
              go to lab-cns-pax-fim
           end-if.
           if ab05-chave = high-values
              perform rot-fim-arquivo
              start arqab05 key is not less ab05-chave-3
              move 0 to codigo
              move 1 to erro
              go to lab-cns-pax-05
           end-if.
      *
       lab-cns-pax-04.
           perform rot-display.
      *
       lab-cns-pax-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-pax-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-pax-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-pax-03
                    when kbd-aux = 73
                         go to lab-cns-pax-01
                    when kbd-aux = 71
                         move low-values to ab05-chave-3
                         go to lab-cns-pax-00-a
                    when kbd-aux = 79
                         move high-values to ab05-chave-3
                         go to lab-cns-pax-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-pax-05
           end-if.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa-cad.
           display tela-limpa.
           move 0 to cons.
           go to lab-cns-pax-00.
      *
       lab-cns-pax-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa.
           exit.
      *
       sec-consulta-empresa section.
      *
       lab-cns-empresa-00.
           move spaces to empresa.
           perform lmp-empresa.
           perform acc-empresa.
           if escape-key = 1
              perform lmp-cons
              go to lab-cns-empresa-fim
           end-if.
           display tela-04.
           move empresa to txt.
           perform rot-texto.
           move low-values to ab05-chave-4.
           move txt to ab05-empresa.
      *
       lab-cns-empresa-00-a.
           start arqab05 key is not less ab05-chave-4.
           go to lab-cns-empresa-03.
      *
       lab-cns-empresa-01.
           perform rot-le-anterior.
           if erro not = 0 or ab05-codigo = codigo
              perform rot-inic-arquivo
              start arqab05 key is not less ab05-chave-4
              move 1 to erro
              go to lab-cns-empresa-05
           end-if.
           if ab05-chave = high-values
              go to lab-cns-empresa-01
           end-if.
           go to lab-cns-empresa-04.
      *
       lab-cns-empresa-02.
           start arqab05 key is less ab05-chave-4.
      *
       lab-cns-empresa-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab05
              go to lab-cns-empresa-fim
           end-if.
           if ab05-chave = high-values
              perform rot-fim-arquivo
              start arqab05 key is not less ab05-chave-4
              move 0 to codigo
              move 1 to erro
              go to lab-cns-empresa-05
           end-if.
      *
       lab-cns-empresa-04.
           perform rot-display.
      *
       lab-cns-empresa-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-empresa-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-empresa-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-empresa-03
                    when kbd-aux = 73
                         go to lab-cns-empresa-01
                    when kbd-aux = 71
                         move low-values to ab05-chave-4
                         go to lab-cns-empresa-00-a
                    when kbd-aux = 79
                         move high-values to ab05-chave-4
                         go to lab-cns-empresa-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-empresa-05
           end-if.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa-cad.
           display tela-limpa.
           move 0 to cons.
           go to lab-cns-empresa-00.
      *
       lab-cns-empresa-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa.
           exit.
      *
       sec-consulta-destino section.
      *
       lab-cns-destino-00.
           move spaces to destino.
           perform lmp-destino.
           perform acc-destino.
           if escape-key = 1
              perform lmp-cons
              go to lab-cns-destino-fim
           end-if.
           display tela-04.
           move destino to txt.
           perform rot-texto.
           move low-values to ab05-chave-5.
           move txt to ab05-destino.
      *
       lab-cns-destino-00-a.
           start arqab05 key is not less ab05-chave-5.
           go to lab-cns-destino-03.
      *
       lab-cns-destino-01.
           perform rot-le-anterior.
           if erro not = 0 or ab05-codigo = codigo
              perform rot-inic-arquivo
              start arqab05 key is not less ab05-chave-5
              move 1 to erro
              go to lab-cns-destino-05
           end-if.
           if ab05-chave = high-values
              go to lab-cns-destino-01
           end-if.
           go to lab-cns-destino-04.
      *
       lab-cns-destino-02.
           start arqab05 key is less ab05-chave-5.
      *
       lab-cns-destino-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab05
              go to lab-cns-destino-fim
           end-if.
           if ab05-chave = high-values
              perform rot-fim-arquivo
              start arqab05 key is not less ab05-chave-5
              move 0 to codigo
              move 1 to erro
              go to lab-cns-destino-05
           end-if.
      *
       lab-cns-destino-04.
           perform rot-display.
      *
       lab-cns-destino-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-destino-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-destino-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-destino-03
                    when kbd-aux = 73
                         go to lab-cns-destino-01
                    when kbd-aux = 71
                         move low-values to ab05-chave-5
                         go to lab-cns-destino-00-a
                    when kbd-aux = 79
                         move high-values to ab05-chave-5
                         go to lab-cns-destino-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-destino-05
           end-if.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa-cad.
           display tela-limpa.
           move 0 to cons.
           go to lab-cns-destino-00.
      *
       lab-cns-destino-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa.
           exit.
      *
       sec-consulta-embarque section.
      *
       lab-cns-embarque-00.
           move 0 to data-emb.
           perform lmp-data-emb.
           perform acc-data-emb.
           if escape-key = 1
              go to lab-cns-embarque-fim
           end-if.
           if data-emb not = 0
              move data-emb to data-aux
              move dia-aux to dia-euro
              move mes-aux to mes-euro
              move ano-aux to ano-euro
              move 4 to opcao-data
              perform rot-data
              if return-code not = 0
                 perform err-data-i
                 go to lab-cns-embarque-00
              end-if
           end-if.
           move low-values to ab05-chave-6.
           move dias-corr to ab05-data-emb.
      *
       lab-cns-embarque-00-a.
           start arqab05 key is not less ab05-chave-6.
           go to lab-cns-embarque-03.
      *
       lab-cns-embarque-01.
           perform rot-le-anterior.
           if erro not = 0 or ab05-codigo = codigo
              perform rot-inic-arquivo
              start arqab05 key is not less ab05-chave-6
              move 1 to erro
              go to lab-cns-embarque-05
           end-if.
           if ab05-chave = high-values
              go to lab-cns-embarque-01
           end-if.
           go to lab-cns-embarque-04.
      *
       lab-cns-embarque-02.
           start arqab05 key is less ab05-chave-6.
      *
       lab-cns-embarque-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab05
              go to lab-cns-embarque-fim
           end-if.
           if ab05-chave = high-values
              perform rot-fim-arquivo
              start arqab05 key is not less ab05-chave-6
              move 0 to codigo
              move 1 to erro
              go to lab-cns-embarque-05
           end-if.
      *
       lab-cns-embarque-04.
           perform rot-display.
      *
       lab-cns-embarque-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-embarque-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-embarque-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-embarque-03
                    when kbd-aux = 73
                         go to lab-cns-embarque-01
                    when kbd-aux = 71
                         move low-values to ab05-chave-6
                         go to lab-cns-embarque-00-a
                    when kbd-aux = 79
                         move high-values to ab05-chave-6
                         go to lab-cns-embarque-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-embarque-05
           end-if.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa-cad.
           display tela-limpa.
           move 0 to cons.
           go to lab-cns-embarque-00.
      *
       lab-cns-embarque-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-obs-2.
           display tela-limpa.
           exit.
      *
       sec-exclusao section.
      *
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
           perform rot-le-ab05-lock.
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
           delete arqab05 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAB05A.DAT - Tecle <Enter>
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
           unlock arqab05 record.
           display tela-04.
           exit.
      *
       sec-alteracao section.
      *
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
           perform rot-le-ab05-lock.
           perform rot-display.
      *
       lab-alt-01.
           display tela-15.
           perform lmp-cons.
           perform acc-cons.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           if escape-key = 3
              perform pesq-cons
              move rotina-ab02-codigo to cons
              go to lab-alt-01
           end-if.
           if cons = 0
              go to lab-alt-01
           end-if.
           move cons to ab02-codigo.
           perform rot-le-ab02.
           if erro not = 0
              perform err-codigo-n
              go to lab-alt-01
           end-if.
           move ab02-razao-social-a to dcons.
           perform dsp-cons.
           if ab02-rateio = "N"
              perform err-rateio
              go to lab-alt-01
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-02.
           perform lmp-data-rec.
           perform acc-data-rec.
           if escape-key = 1
              go to lab-alt-01
           end-if.
           if data-rec = 0
              go to lab-alt-02
           end-if.
           move data-rec to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-alt-02
           end-if.
           move data-disp to data-rec-disp.
           move dias-corr to data-rec
           perform dsp-data-rec.
      *
       lab-alt-03.
           perform acc-pax.
           if escape-key = 1
              move data-rec to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-rec
              go to lab-alt-02
           end-if.
           move pax to txt pax-a.
           perform rot-texto.
           if txt = spaces 
              go to lab-alt-03
           end-if.
           move txt to pax.
      *
       lab-alt-04.
           perform acc-empresa.
           if escape-key = 1
              move pax-a to pax
              go to lab-alt-03
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              move spaces to empresa empresa-a
              perform dsp-empresa
           else
              move txt to empresa
           end-if.
      *
       lab-alt-05.
           display tela-08.
           perform acc-destino.
           if escape-key = 1
              move empresa-a to empresa
              display tela-limpa-cad
              go to lab-alt-04
           end-if.
           if escape-key = 3
              perform rot-pesq-cid
              move rotina-cid to destino
              go to lab-alt-05
           end-if.
           move destino to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-05
           end-if.
           move txt to destino.
           perform dsp-destino.
           move 08 to wtab04-tipo.
           move destino to wtab04-sigla.
           move spaces to wtab04-resto.
           move wtab04-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-cidade-n
              go to lab-alt-05
           end-if.
           move reg-tabl to reg-wtab04.
           display tela-limpa-cad.
      *
       lab-alt-06.
           perform lmp-data-emb.
           perform acc-data-emb.
           if escape-key = 1
              go to lab-alt-05
           end-if.
           if data-emb = 0
              perform lmp-data-emb
              go to lab-alt-07
           end-if.
           move data-emb to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-alt-06
           end-if.
           if dias-corr < data-rec
              perform err-data-emb
              go to lab-alt-06
           end-if.
           move data-disp to data-emb-disp.
           move dias-corr to data-emb
           perform dsp-data-emb.
      *
       lab-alt-07.
           perform lmp-horario-e.
           perform acc-horario-e.
           if escape-key = 1
              if data-emb not = 0
                 move data-emb to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to data-emb
              end-if
              go to lab-alt-06
           end-if.
           move horario-e to horario-r horario-e-disp.
           if hora-r > 23 or min-r > 59
              go to lab-alt-07
           end-if.
           perform dsp-horario-e.
      *
       lab-alt-08.
           perform acc-voo.
           if escape-key = 1 
              go to lab-alt-07
           end-if.
      *
       lab-alt-09.
           display tela-12
           perform lmp-cia.
           perform acc-cia.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-08
           end-if.
           if escape-key = 3
              perform rot-pesq-cia
              move rotina-cia to cia
              go to lab-alt-09
           end-if.
           move cia to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-09
           end-if.
           move txt to cia.
           move spaces to dcia.
           perform dsp-cia.
           move 09 to wtab05-tipo.
           move cia to wtab05-sigla.
           move spaces to wtab05-resto.
           move wtab05-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-cia-n
              go to lab-alt-09
           end-if.
           move reg-tabl to reg-wtab05.
           move wtab05-descricao to dcia.
           perform dsp-cia.
           display tela-limpa-cad.
      *
       lab-alt-10.
           display tela-16.
           perform acc-aerop.
           if escape-key = 1
              go to lab-alt-09
           end-if.
           if aerop not = 1 and 2
              go to lab-alt-10
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-11.
           perform lmp-data-ent.
           perform acc-data-ent.
           if escape-key = 1
              go to lab-alt-10
           end-if.
           if data-ent = 0
              perform lmp-data-ent
              go to lab-alt-14
           end-if.
           move data-ent to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-alt-11
           end-if.
           if dias-corr < data-rec
              perform err-data-ent
              go to lab-alt-11
           end-if.
           move data-disp to data-ent-disp.
           move dias-corr to data-ent
           perform dsp-data-ent.
      *
       lab-alt-12.
           perform lmp-horario.
           perform acc-horario.
           if escape-key = 1
              move data-ent to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-ent
              go to lab-alt-11
           end-if.
           move horario to horario-r horario-disp.
           if hora-r > 23 or min-r > 59
              go to lab-alt-12
           end-if.
           perform dsp-horario.
      *
       lab-alt-13.
           display tela-14.
           perform acc-atendente.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-12
           end-if.
           if escape-key = 3
              perform rot-pesq-usr
              move rotina-usr to atendente
              go to lab-alt-13
           end-if.
           move atendente to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-13
           end-if.
           move txt to atendente usr-usuario.
           perform dsp-atendente.
           perform rot-le-usr.
           if erro not = 0
              perform err-atendente-n
              go to lab-alt-13
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-14.
           perform acc-obs-1.
           if escape-key = 1
              if data-ent = 0
                 go to lab-alt-11
              else
                 go to lab-alt-13
             end-if
           end-if.
           if obs (01) = spaces
              go to lab-alt-16
           end-if.
      *
       lab-alt-15.
           perform acc-obs-2.
           if escape-key = 1
              go to lab-alt-14
           end-if.
      *
       lab-alt-16.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-15
           end-if.
           if resposta = "N"
              move pax-a to pax
              move empresa-a to empresa
              move data-rec to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-rec
              if data-emb not = 0
                 move data-emb to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to data-emb
              end-if
              if data-ent not = 0
                 move data-ent to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to data-ent
              end-if
              display tela-limpa-cad
              go to lab-alt-01
           else
              if resposta not = "S"
                 go to lab-alt-16
              end-if
           end-if.
           perform rot-move-ab05.
           rewrite reg-ab05 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAB05A.DAT - Tecle <Ent
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
           unlock arqab05 record.
           display tela-04.
           exit.
