      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  QGBX01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Conversao do cadastro de Baixas :                          *
      *                                                             *
      *  Data da ultima alteracao:    23/02/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. qgbx01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqbx01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is bx01-chave
                  alternate record key is bx01-chave-1 with duplicates
                  alternate record key is bx01-chave-2 with duplicates
                  file status is bx01-status.
      *
           select arqbx99 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is bx99-chave
                  alternate record key is bx99-chave-1 with duplicates
                  alternate record key is bx99-chave-2 with duplicates
                  file status is bx99-status.
      *
       data division.
       file section.
      *    
       copy fdbx01.lib.
      *    
       copy fdbx99.lib.
      *
       working-storage section.
      *
       01 bx01-status                  pic x(02) value "00".
       01 bx01-stat                    pic x(01) value "F".
      *
       01 nome-arq-bx01.
          02 bx01-dir                  pic x(03) value "RC2".
          02 filler                    pic x(01) value "\".
          02 bx01-nome                 pic x(08) value "ARQBX01A".
          02 filler                    pic x(01) value ".".
          02 bx01-ext                  pic x(03) value "DAT".
      *
       01 bx99-status                  pic x(02) value "00".
       01 bx99-stat                    pic x(01) value "F".
      *
       01 nome-arq-bx99.
          02 bx99-dir                  pic x(03) value "RC2".
          02 filler                    pic x(01) value "\".
          02 bx99-nome                 pic x(08) value "ARQBX99A".
          02 filler                    pic x(01) value ".".
          02 bx99-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "QGBX01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       procedure division.
      *
       lab-00.
           open input arqbx99.
           move high-values to bx99-chave.
           read arqbx99.
           open output arqbx01.
           move zeros to reg-bx01-1.
           move high-values to bx01-chave-controle.
           move bx99-ult-bx to bx01-ult-bx.
           write reg-bx01-1
           move low-values to bx99-chave.
           start arqbx99 key is not less bx99-chave.
       lab-01.
           read arqbx99 next at end go to lab-fim.
           if bx99-chave = high-values go to lab-01.
           move reg-bx99 to reg-bx01.
           write reg-bx01.
           display bx01-codigo "  " bx01-documento.
           go to lab-01.
       lab-fim.
           close arqbx01 arqbx99.
           stop run.
