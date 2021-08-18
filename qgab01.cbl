      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  QGAB01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Conversao do cadastro de Associados :                      *
      *                                                             *
      *  Data da ultima alteracao:    23/02/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. qgab01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
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
           select arqab99 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab99-chave
                  alternate record key is ab99-chave-1 with duplicates
                  alternate record key is ab99-chave-2 with duplicates
                  file status is ab99-status.
      *
       data division.
       file section.
      *    
       copy fdab01.lib.
      *    
       copy fdab99.lib.
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
       01 ab99-status                  pic x(02) value "00".
       01 ab99-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab99.
          02 ab99-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab99-nome                 pic x(08) value "ARQAB99A".
          02 filler                    pic x(01) value ".".
          02 ab99-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "QGAB01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       procedure division.
      *
       lab-00.
           open input arqab99.
           open output arqab01.
           move zeros to reg-ab01-1.
           move high-values to ab01-chave-controle.
           write reg-ab01-1
           move low-values to ab99-chave.
           start arqab99 key is not less ab99-chave.
       lab-01.
           read arqab99 next at end go to lab-fim.
           if ab99-chave = high-values go to lab-01.
           move reg-ab99 to reg-ab01.
           move spaces to ab01-titular.
           write reg-ab01.
           display ab01-codigo "  " ab01-razao-social-a.
           go to lab-01.
       lab-fim.
           close arqab01 arqab99.
           stop run.
