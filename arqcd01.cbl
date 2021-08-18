      $set ans85
      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGCD01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro :                                   *
      *                                                             *
      *  Data da ultima alteracao:    18/04/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgcd01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
            select arqcd01 assign to disk
                   organization is indexed
                   access mode is dynamic
                   lock mode is manual
                   with lock on record
                   record key is cd01-chave
                   alternate record key is cd01-chave-1 with duplicates
                   alternate record key is cd01-chave-2 with duplicates
                   alternate record key is cd01-chave-3 with duplicates
                   alternate record key is cd01-chave-4 with duplicates
                   file status is cd01-status.
      
       data division.
       file section.
           
       copy fdcd01.lib.
      
       working-storage section.
      
       01 cd01-status                  pic x(02) value "00".
       01 cd01-stat                    pic x(01) value "F".
      *
       01 nome-arq-cd01.
          02 cd01-dir                  pic x(03) value "CD1".
          02 filler                    pic x(01) value "\".
          02 cd01-nome                 pic x(08) value "ARQCD01A".
          02 filler                    pic x(01) value ".".
          02 cd01-ext                  pic x(03) value "DAT".
      *
       procedure division.
      *
       lab-00.
           open output arqcd01.
           move zeros to reg-cd01.
           move high-values to cd01-chave-controle.
           move 0 to cd01-numero.
           write reg-cd01-1.
           close arqcd01.