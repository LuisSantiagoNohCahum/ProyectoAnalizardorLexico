#Importamos las librerias, es importante tener la libreria tabulate para poder mostrar la tabla con formato
from numpy.core import numeric
import ply.lex as lex
import sys
import pandas as pd
from tabulate import tabulate

#definimos los tokens para el analizador lexico
tokens = (
    #DEFINIMOS LAS PAPABRAS RESERVADAS
    #ORDENADAS EN ORDEN ALFABETICO
    # A
    'ACCEPT', 'ACCESS', 'ADD', 'ADDRESS', 'ADVANCING', 'AFTER', 'ALL', 'ALPHABET', 
    'ALPHABETIC', 'ALPHABETIC_LOWER', 'ALPHABETIC_UPPER', 'APHANUMERIC', 'ALPHANUMERIC_EDITED', 
    'ALSO', 'ALTER', 'ALTERNATE', 'AND', 'ANY', 'APPLY', 'ARE', 'AREA', 'AREAS', 'ASCENDING', 
    'ASSIGN', 'AT', 'AUTHOR', 'AUTO', 'AUTO_SKIP', 'AUTOMATIC', 'AUTOTERMINATE',

    # B
    'BACKGROUND', 'BACKGROUND_COLOR', 'BACKGROUND_HIGH', 'BACKGROUND_LOW', 'BACKWARD', 
    'BEEP', 'BEFORE', 'BELL', 'BIND', 'BINARY', 'BLANK', 'BLINK', 'BLINKING', 'BLOCK', 
    'BOLD', 'BOTTOM', 'BY',

    # C
    'CALL', 'CANCEL', 'CD', 'CF', 'CH', 'CHARACTER', 'CHARACTERS', 'CLASS', 'CLOSE', 'COBOL', 
    'CODE', 'CODE_SET', 'COL', 'COLLATING', 'COLUMN', 'COMMA', 'COMMUNICATION', 'COMP', 'COMP_1', 
    'COMP_2', 'COMP_3', 'COMP_4', 'COMP_6', 'COMPUTATIONAL', 'COMPUTATIONAL_1', 'COMPUTATIONAL_2', 
    'COMPUTATIONAL_3', 'COMPUTATIONAL_4', 'COMPUTE', 'CONFIGURATION', 'CONSOLE', 'CONTAINS', 'CONTENT', 
    'CONTINUE', 'CONTROL', 'CONTROLS', 'CONVERSION', 'CONVERT', 'CONVERTING', 'COPY', 'CORR', 
    'CORRESPONDING', 'COUNT', 'CRT', 'CURRENCY', 'CURSOR',

    # D
    'DATA', 'DATE', 'DATE_COMPILED', 'DATE_WRITTEN', 'DAY', 'DAY_OF_WEEK', 'DE', 'DEBUG', 'DEBUGGING', 
    'DECIMAL_POINT', 'DECLARATIVES', 'DEFAULT', 'DELETE', 'DELIMITED', 'DELIMITER', 'DEPENDING', 'DESCENDING', 
    'DESTINATION', 'DETAIL', 'DISABLE', 'DISPLAY', 'DIVIDE', 'DIVISION', 'DRAWN', 'DUPLICATES', 'DYNAMIC',

    # E
    'ECHO', 'EGI', 'ELSE', 'EMI', 'EMPTY_CHECK', 'ENABLED', 'END', 'END_ACCEPT', 'END_ADD', 'END_CALL', 
    'END_COMPUTE', 'END_DELETE', 'END_DIVIDE', 'END_EVALUATEQ', 'END_IF', 'END_MULTIPLY', 'END_OF_PAGE', 
    'END_PERFORM', 'END_READ', 'END_RECEIVE', 'END_RETURN', 'END_REWRITE', 'END_SEARCH', 'END_START', 
    'END_STRING', 'END_SUBTRACT', 'END_UNSTRING', 'END_WRITE', 'ENTER', 'ENVIRONMENT', 'EOL', 'EOP', 'EOS', 
    'EQUAL', 'ERASE', 'ERROR', 'ESCAPE', 'ESI', 'EVALUATE', 'EVERY', 'EXCEPTION', 'EXCLUSIVE', 'EXIT', 
    'EXTEND', 'EXTERNAL',

    # F
    'FALSE', 'FD', 'FILE', 'FILE_CONTROL', 'FILE_ID', 'FILE_PREFIX', 'FILLER', 'FINAL', 'FIRST', 'FOOTING', 
    'FOR', 'FOREGROUND_COLOR', 'FOREGROUND_COLOUR', 'FROM', 'FULL','FUNCTION',

    # G
    'GENERATE', 'GIVING', 'GLOBAL', 'GO', 'GOBACK', 'GREATER', 'GRID', 'GROUP',

    # H
    'HEADING', 'HIGH', 'HIGH_BALUE', 'HIGH_VALUES', 'HIGHLIGHT',  

    # I
    'I_O', 'IDENTIFICATION', 'IF', 'IN', 'INDEX', 'INDEXED', 'INDICATE', 'INITIAL', 'INITIALIZE', 
    'INPUT', 'INPUT_OUTPUT', 'INSPECT', 'INSTALLATION', 'INTO', 'INVALID', 'IS',
    
    # J
    'JUST', 'JUSTIFIED',

    # K
    'KEY',

    #I
    #'IDENTIFICATION', 'DIVISION', 'PROGRAM_ID', 'AUTHOR','INSTALLATION','DATE_WRITTEN', 'DATE_COMPILED', 'SECURITY', 
    #'CONFIGURATION', 'SECTION', 'SOURCE_COMPUTER', 'OBJECT_COMPUTER', 'DATA', 'WORKING_STORAGE','PIC', 'PROCEDURE',
    #'DISPLAY', 'ACCEPT', 'STOP', 'RUN','WITH','NO','ADVANCING', 'FUNCTION', 'LENGTH',
    
    # L
    'LABEL', 'LAST', 'LEADING', 'LEFT', 'LEFTLINE', 'LENGTH', 'LENGTH_CHECK', 'LESS', 'LIMIT', 'LIMITS', 
    'LINAGE', 'LINGAKE_COUNTER', 'LINES', 'LINKAGE', 'LOCK', 'LOCK_HOLDING', 'LOW', 'LOW_VALUE', 
    'LOW_VALUES', 'LOWLIGHT',

    # M
    'MEMORY', 'MERGE', 'MODE', 'MODULES', 'MOVE', 'MULTIPLE', 'MULTIPLY',

    # N
    'NATIVE', 'NEGATIVE', 'NEXT', 'NO', 'NO_ECHO', 'NOT', 'NUMBER', 'NUMERIC', 'NUMERIC_EDITED', 
    
    # O
    'OBJECT_COMPUTER', 'OCCURS', 'OF', 'OFF', 'OMITTED', 'ON', 'OPEN', 'OPTIONAL', 'OR', 'ORDER', 
    'ORGANIZATION', 'OTHER', 'OTHERS', 'OUTPUT', 'OVERFLOW', 'OVERLINE',

    # P
    'PACKED_DECIMAL', 'PADDING', 'PGE', 'PAGE_COUNTER', 'PERFORM', 'PIC', 'PICTURE', 'PLUS', 'POINTER', 
    'POS', 'POSITION', 'POSITIVE', 'PREVIOUS', 'PRINT_CONTROL', 'PRINTING', 'PROCEDURE', 'PROCEDURES', 
    'PROCEED', 'PROGRAM', 'PROGRAM_ID', 'PROMPT', 'PROTECTED', 'PURGE',

    # Q
    'QUEUE', 'QUOTE', 'QUOTES',

    # R
    'RANDOM', 'RD', 'READ', 'READERS', 'RECEIVE', 'RECORD', 'RECORDING', 'RECORDS', 'REDEFINES', 'REEL', 
    'REFERENCE', 'REFERENCES', 'RELATIVE', 'RELEASE', 'REMAINDER', 'REMOVAL', 'RENAMES', 'REPLACE', 
    'REPLACING', 'REQUIRED', 'REPORT', 'REPORTING', 'REPORTS', 'RERUN', 'RESERVE', 'RETURN', 'RETUNRNIN', 
    'RETURN_CODE', 'RETURN_UNSIGNED', 'REVERSE', 'REVERSE_VIDEO', 'REVERSED', 'REWIND', 'REWRITE', 'RF', 
    'RH', 'RIGHT', 'ROLLBACK', 'RUN',

    # S
    'SAME', 'SCREEN', 'SD', 'SEARCH', 'SECTION', 'SECURE', 'SECURITY', 'SEGMENT', 'SEGMENT_LIMIT', 'SELECT', 
    'SEND', 'SENTENCE', 'SEPARATE', 'SEQUENCE', 'SEQUENTIAL', 'SET', 'SIGN', 'SIZE', 'SORT', 'SORT_MERGE', 
    'SOURCE', 'SOURCE_COMPUTER', 'SPACE', 'SPACES', 'SPECIAL_NAMES', 'STANDARD', 'START', 'STATUS', 'STOP', 
    'SUBTRACT', 'SUPPRESS', 'SYMBOLIC', 'SYNC', 'SYCHRONIZED',

    # T
    'TAB', 'TALLYNG', 'TAPE', 'TERMINAL', 'TERMINATE', 'TEST', 'TEXT', 'THAN', 'THEN', 'THROUGH', 'THRU', 
    'TIME', 'TIMES', 'TO', 'TOP', 'TRAILING', 'TRUE', 

    # U
    'UNDERLINE', 'UNDERLINED', 'UNIT', 'UNLOCK', 'UNSTRING', 'UNTIL', 'UP', 'UPDATE', 'UPDATES', 'UPON', 
    'USAGE', 'USE', 'USING',

    # V
    'VALUE', 'VALUES', 'VARYING',

    # W
    'WHEN', 'WITH', 'WORDS', 'WORKING_STORAGE', 'WRITE', 'WRITERS', 
    
    # Z
    'ZERO', 'ZEROS', 'ZEROES', 

    #booleanos
    #'TRUE','FALSE',

    # SIMBOLOS
    'SUMA','INCREMENTO','MAS_IGUAL','RESTA','DECREMENTO','MENOS_IGUAL','MULTIPLICACION',
    'EXPONENTE','OP_DIVISION','RESIDUO','MENOR_QUE','MENOR_IGUAL','MAYOR_QUE','MAYOR_IGUAL','IGUAL',
    'DESIGUAL','DISTINTO','ES_IGUAL','PUNTOYCOMA','COMA','PARENT_IZQ','PARENT_DER','CORCHETE_IZQ',
    'CORCHETE_DER','LLAVE_IZQ','LLAVE_DER','DOS_PUNTOS','ET','HASHTAG','PUNTO','COMILLAS',
    'APOSTROFE', 'IGUALDAD_ESTRICTA', 'DESIGUALDAD_ESTRICTA',

    #OPERADORES LOGICOS
    'AND_LOGICO','OR_LOGICO','NOT_LOGICO',

    #OPERADORES DE ASIGNACION
    'ASIGNACION_MULTI','ASIGNACION_DIVISION','ASIGNACION_RESIDUO','ASIGNACION_SUMA',
    'ASIGNACION_RESTA',

    # OTROS
    'COMMENTS','COMMENTS_C99','VARIABLE','IDENTIFICADOR','NUMERO','STRING',
    )

#DEFINIMOS LOS METODOS CON LAS EXPRESIONES REGULARES PARA CADA UNO DE LOS TOKENS DEFINIDOS ANTERIORMENTE
t_ignore = " \t" #ER para ignorar tabuladores

#funcion para nueva linea
def t_newline(t): 
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
#funcion para errores lexicos
def t_error(t):     
    print (chr(27)+"[1;31m"+"\t ERROR: CARÁCTER ILEGAL"+chr(27)+"[0m")
    print ("\t\tLine: "+str(t.lexer.lineno)+"\t=> " + t.value[0])
    t.lexer.skip(1)
    
#Declaracion de las funciones y expresiones regulares para cada palabra reservada definida anteriormente 
#En orden alfabetico
# A
def t_AUTHOR(t):
    r'AUTHOR'
    return t
def t_ACCEPT (t):
    r'ACCEPT'
    return t
def t_ADVANCING (t):
    r'ADVANCING'
    return t
def t_ACCESS (t):
    r'ACCESS'
    return t
def t_ADD (t):
    r'ADD'
    return t
def t_ADDRESS (t):
    r'ADDRESS'
    return t
def t_AFTER (t):
    r'AFTER'
    return t
def t_ALL (t):
    r'ALL'
    return t
def t_ALPHABET (t):
    r'ALPHABET'
    return t
def t_ALPHABETIC (t):
    r'ALPHABETIC'
    return t
def t_APHANUMERIC (t):
    r'APHANUMERIC'
    return t
def t_ALSO (t):
    r'ALSO'
    return t
def t_ALTER (t):
    r'ALTER'
    return t
def t_ALTERNATE (t):
    r'ALTERNATE'
    return t
def t_AND (t):
    r'AND'
    return t
def t_ANY (t):
    r'ANY'
    return t
def t_APPLY (t):
    r'APPLY'
    return t   
def t_ARE (t):
    r'ARE'
    return t
def t_AREA (t):
    r'AREA'
    return t
def t_AREAS (t):
    r'AREAS'
    return t
def t_ASCENDING (t):
    r'ASCENDING'
    return t
def t_ASSIGN (t):
    r'ASSIGN'
    return t
def t_AT (t):
    r'AT'
    return t
def t_AUTO (t):
    r'AUTO'
    return t
def t_AUTOMATIC (t):
    r'AUTOMATIC'
    return t  
def t_AUTOTERMINATE (t):
    r'AUTOTERMINATE'
    return t


# B
def t_BACKGROUND (t):
    r'BACKGROUND'
    return t
def t_BACKWARD (t):
    r'BACKWARD'
    return t
def t_BEFORE (t):
    r'BEFORE'
    return t
def t_BEEP (t):
    r'BEEP'
    return t
def t_BELL (t):
    r'BELL'
    return t
def t_BIND (t):
    r'BIND'
    return t
def t_BINARY (t):
    r'BINARY'
    return t
def t_BLANK (t):
    r'BLANK'
    return t
def t_BLINK (t):
    r'BLINK'
    return t
def t_BLINKING (t):
    r'BLINKING'
    return t
def t_BLOCK (t):
    r'BLOCK'
    return t
def t_BOLD (t):
    r'BOLD'
    return t
def t_BOTTOM (t):
    r'BOTTOM'
    return t
def t_BY (t):
    r'BY'
    return t


# C
def t_CONFIGURATION(t):
    r'CONFIGURATION'
    return t
def t_CALL (t):
    r'CALL'
    return t
def t_CANCEL (t):
    r'CANCEL'
    return t
def t_CD (t):
    r'CD'
    return t
def t_CF (t):
    r'CF'
    return t
def t_CHARACTER (t):
    r'CHARACTER'
    return t
def t_CHARACTERS (t):
    r'CHARACTERS'
    return t
def t_CLASS (t):
    r'CLASS'
    return t
def t_CLOSE (t):
    r'CLOSE'
    return t
def t_COBOL (t):
    r'COBOL'
    return t
def t_CODE (t):
    r'CODE'
    return t
def t_COL (t):
    r'COL'
    return t
def t_COLLATING (t):
    r'COLLATING'
    return t
def t_COLUMN (t):
    r'COLUMN'
    return t
def t_COMMA (t):
    r'COMMA'
    return t
def t_COMMUNICATION (t):
    r'COMMUNICATION'
    return t
def t_COMP (t):
    r'COMP'
    return t
def t_COMPUTATIONAL (t):
    r'COMPUTATIONAL'
    return t
def t_CONSOLE (t):
    r'CONSOLE'
    return t
def t_CONTAINS (t):
    r'CONTAINS'
    return t
def t_CONTINUE (t):
    r'CONTINUE'
    return t
def t_CONTROL (t):
    r'CONTROL'
    return t
def t_CONTROLS (t):
    r'CONTROLS'
    return t
def t_CONVERSION (t):
    r'CONVERSION'
    return t
def t_CONVERT (t):
    r'CONVERT'
    return t
def t_CONVERTING (t):
    r'CONVERTING'
    return t
def t_COPY (t):
    r'COPY'
    return
def t_CORR (t):
    r'CORR'
    return t
def t_CORRESPONDING (t):
    r'CORRESPONDING'
    return t
def t_COUNT (t):
    r'COUNT'
    return t
def t_CURRENCY (t):
    r'CURRENCY'
    return t
def t_CURSOR (t):
    r'CURSOR'
    return t
def t_CRT (t):
    r'CRT'
    return t


# D
def t_DIVISION(t):
    r'DIVISION'
    return t
def t_DATE_WRITTEN(t):
    r'DATE-WRITTEN'
    return t
def t_DATE_COMPILED(t):
    r'DATE-COMPILED'
    return t
def t_DATA(t):
    r'DATA'
    return t
def t_DISPLAY(t):
    r'DISPLAY'
    return t
def t_DATE (t):
    r'DATE'
    return t
def t_DAY (t):
    r'DAY'
    return t
def t_DEBUGGING (t):
    r'DEBUGGING'
    return t
def t_DEBUG (t):
    r'DEBUG'
    return t
def t_DE (t):
    r'DE'
    return t
def t_DECLARATIVES (t):
    r'DECLARATIVES'
    return t
def t_DEFAULT (t):
    r'DEFAULT'
    return t
def t_DELETE (t):
    r'DELETE'
    return t
def t_DELIMITED (t):
    r'DELIMITED'
    return t
def t_DELIMITER (t):
    r'DELIMITER'
    return t
def t_DEPENDING (t):
    r'DEPENDING'
    return t
def t_DESTINATION (t):
    r'DESTINATION'
    return t
def t_DETAIL (t):
    r'DETAIL'
    return t
def t_DISABLE (t):
    r'DISABLE'
    return t
def t_DIVIDE (t):
    r'DIVIDE'
    return t
def t_DRAWN (t):
    r'DRAWN'
    return t
def t_DUPLICATES (t):
    r'DUPLICATES'
    return t
def t_DYNAMIC (t):
    r'DYNAMIC'
    return t


# E
def t_ECHO (t):
    r'ECHO'
    return t
def t_EGI (t):
    r'EGI'
    return t
def t_ELSE (t):
    r'ELSE'
    return t
def t_EMI (t):
    r'EMI'
    return t
def t_ENABLED (t):
    r'ENABLED'
    return t
def t_END (t):
    r'END'
    return t
def t_ENTER (t):
    r'ENTER'
    return t
def t_ENVIRONMENT (t):
    r'ENVIRONMENT'
    return t
def t_EOL (t):
    r'EOL'
    return t
def t_EOP (t):
    r'EOP'
    return t
def t_EOS(t):
    r'EOS'
    return t
def t_EQUAL (t):
    r'EQUAL'
    return t
def t_ERASE (t):
    r'ERASE'
    return t
def t_ERROR (t):
    r'ERROR'
    return t
def t_ESCAPE (t):
    r'ESCAPE'
    return t
def t_ESI (t):
    r'ESI'
    return t
def t_EVALUATE (t):
    r'EVALUATE'
    return t
def t_EVERY (t):
    r'EVERY'
    return t
def t_EXCEPTION (t):
    r'EXCEPTION'
    return t
def t_EXCLUSIVE (t):
    r'EXCLUSIVE'
    return t
def t_EXIT (t):
    r'EXIT'
    return t
def t_EXTEND (t):
    r'EXTEND'
    return t
def t_EXTERNAL (t):
    r'EXTERNAL'
    return t


# F
def t_FUNCTION (t):
    r'FUNCTION'
    return t
def t_FALSE (t):
    r'FALSE'
    return t
def t_FD (t):
    r'FD'
    return t
def t_FILE (t):
    r'FILE'
    return t
def t_FILLER (t):
    r'FILLER'
    return t
def t_FINAL (t):
    r'FINAL'
    return t
def t_FIRST (t):
    r'FIRST'
    return t
def t_FOOTING (t):
    r'FOOTING'
    return t
def t_FOR (t):
    r'FOR'
    return t
def t_FROM (t):
    r'FROM'
    return t
def t_FULL (t):
    r'FULL'
    return t


# G
def t_GENERATE (t):
    r'GENERATE'
    return t
def t_GIVING (t):
    r'GIVING'
    return t
def t_GLOBAL (t):
    r'GLOBAL'
    return t
def t_GO (t):
    r'GO'
    return t
def t_GOBACK (t):
    r'GOBACK'
    return t
def t_GREATER (t):
    r'GREATER'
    return t
def t_GRID (t):
    r'GRID'
    return t
def t_GROUP (t):
    r'GROUP'
    return t


# H
def t_HEADING (t):
    r'HEADING'
    return t
def t_HIGH (t):
    r'HIGH'
    return t
def t_HIGHLIGHT (t):
    r'HIGHLIGHT'
    return t



# I
def t_IDENTIFICATION(t):
    r'IDENTIFICATION'
    return t
def t_INSTALLATION(t):
    r'INSTALLATION'
    return t
def t_IF (t):
    r'IF'
    return t
def t_IN (t):
    r'IN'
    return t
def t_INDEX (t):
    r'INDEX'
    return t
def t_INDEXED (t):
    r'INDEXED'
    return t
def t_INDICATE (t):
    r'INDICATE'
    return t
def t_INITIAL (t):
    r'INITIAL'
    return t
def t_INITIALIZE (t):
    r'INITIALIZE'
    return t
def t_INPUT (t):
    r'INPUT'
    return t
def t_INSPECT (t):
    r'INSPECT'
    return t
def t_INTO (t):
    r'INTO'
    return t
def t_INVALID (t):
    r'INVALID'
    return t
def t_IS (t):
    r'IS'
    return t



# J
def t_JUST (t):
    r'JUST'
    return t
def t_JUSTIFIED (t):
    r'JUSTIFIED'
    return t



# K
def t_KEY (t):
    r'KEY'
    return t


# L
def t_LENGTH (t):
    r'LENGTH'
    return t
def t_LABEL (t):
    r'LABEL'
    return t
def t_LAST (t):
    r'LAST'
    return t
def t_LEADING (t):
    r'LEADING'
    return t
def t_LEFT (t):
    r'LEFT'
    return t
def t_LEFTLINE (t):
    r'LEFTLINE'
    return t
def t_LESS (t):
    r'LESS'
    return t
def t_LIMIT (t):
    r'LIMIT'
    return t
def t_LIMITS (t):
    r'LIMITS'
    return t
def t_LINAGE (t):
    r'LINAGE'
    return t
def t_LINES (t):
    r'LINES'
    return t
def t_LINKAGE (t):
    r'LINKAGE'
    return t
def t_LOCK (t):
    r'LOCK'
    return t
def t_LOW (t):
    r'LOW'
    return t
def t_LOWLIGHT (t):
    r'LOWLIGHT'
    return t


# M
def t_MEMORY (t):
    r'MEMORY'
    return t
def t_MERGE (t):
    r'MERGE'
    return t
def t_MODE (t):
    r'MODE'
    return t
def t_MODULES (t):
    r'MODULES'
    return t
def t_MOVE (t):
    r'MOVE'
    return t
def t_MULTIPLE (t):
    r'MULTIPLE'
    return t
def t_MULTIPLY (t):
    r'MULTIPLY'
    return t

    
# N
def t_NO (t):
    r'NO'
    return t
def t_NATIVE (t):
    r'NATIVE'
    return t
def t_NEGATIVE (t):
    r'NEGATIVE'
    return t
def t_NEXT (t):
    r'NEXT'
    return t
def t_NOT (t):
    r'NOT'
    return t
def t_NUMBER (t):
    r'NUMBER'
    return t
def t_NUMERIC (t):
    r'NUMERIC'
    return t


# O
def t_OBJECT_COMPUTER(t):
    r'OBJECT-COMPUTER'
    return t
def t_OCCURS (t):
    r'OCCURS'
    return t
def t_OF (t):
    r'OF'
    return t
def t_OFF (t):
    r'OFF'
    return t
def t_OMITTED (t):
    r'OMITTED'
    return t
def t_ON (t):
    r'ON'
    return t
def t_OPEN (t):
    r'OPEN'
    return t
def t_OPTIONAL (t):
    r'OPTIONAL'
    return t
def t_OR (t):
    r'OR'
    return t
def t_ORDER (t):
    r'ORDER'
    return t
def t_ORGANIZATION (t):
    r'ORGANIZATION'
    return t
def t_OTHER (t):
    r'OTHER'
    return t
def t_OTHERS (t):
    r'OTHERS'
    return t
def t_OUTPUT (t):
    r'OUTPUT'
    return t
def t_OVERFLOW (t):
    r'OVERFLOW'
    return t
def t_OVERLINE (t):
    r'OVERLINE'
    return t


# P
def t_PROGRAM_ID(t):
    r'PROGRAM-ID'
    return t
def t_PIC(t):
    r'PIC'
    return t
def t_PROCEDURE(t):
    r'PROCEDURE'
    return t
def t_PADDING (t):
    r'PADDING'
    return t
def t_PGE (t):
    r'PGE'
    return t
def t_PERFORM (t):
    r'PERFORM'
    return t
def t_PICTURE (t):
    r'PICTURE'
    return t
def t_PLUS (t):
    r'PLUS'
    return t
def t_POINTER (t):
    r'POINTER'
    return t
def t_POS (t):
    r'POS'
    return t
def t_POSITION (t):
    r'POSITION'
    return t
def t_POSITIVE (t):
    r'POSITIVE'
    return t
def t_PREVIOUS (t):
    r'PREVIOUS'
    return t
def t_PRINTING (t):
    r'PRINTING'
    return t
def t_PROCEDURES (t):
    r'PROCEDURES'
    return t
def t_PROCEED (t):
    r'PROCEED'
    return t

def t_PROTECTED (t):
    r'PROTECTED'
    return t
def t_PROMPT (t):
    r'PROMPT'
    return t
def t_PURGE (t):
    r'PURGE'
    return t


# Q
def t_QUEUE (t):
    r'QUEUE'
    return t
def t_QUOTE (t):
    r'QUOTE'
    return t
def t_QUOTES (t):
    r'QUOTES'
    return t


# R
def t_RUN (t):
    r'RUN'
    return t
def t_RD (t):
    r'RD'
    return t
def t_READ (t):
    r'READ'
    return t
def t_READERS (t):
    r'READERS'
    return t
def t_RECEIVE (t):
    r'RECEIVE'
    return t
def t_RECORD (t):
    r'RECORD'
    return t
def t_RECORDS (t):
    r'RECORDS'
    return t
def t_REDEFINES (t):
    r'REDEFINES'
    return t
def t_REEL (t):
    r'REEL'
    return t
def t_REFERENCE (t):
    r'REFERENCE'
    return t
def t_REFERENCES (t):
    r'REFERENCES'
    return t
def t_RELATIVE (t):
    r'RELATIVE'
    return t
def t_RELEASE (t):
    r'RELEASE'
    return t
def t_REMAINDER (t):
    r'REMAINDER'
    return t
def t_REMOVAL (t):
    r'REMOVAL'
    return t
def t_RENAMES (t):
    r'RENAMES'
    return t
def t_REPLACE (t):
    r'RUN'
    return t
def t_REPLACING (t):
    r'REPLACING'
    return t
def t_REQUIRED (t):
    r'REQUIRED'
    return t
def t_REPORT (t):
    r'REPORT'
    return t
def t_REPORTING (t):
    r'REPORTING'
    return t
def t_REPORTS (t):
    r'REPORTS'
    return t
def t_RERUN (t):
    r'RERUN'
    return t
def t_RESERVE (t):
    r'RESERVE'
    return t
def t_RETURN (t):
    r'RETURN'
    return t
def t_RETUNRNIN (t):
    r'RETUNRNIN'
    return t
def t_REVERSE (t):
    r'REVERSE'
    return t
def t_REVERSED (t):
    r'REVERSED'
    return t
def t_REWIND (t):
    r'REWIND'
    return t
def t_REWRITE (t):
    r'REWRITE'
    return t
def t_ROLLBACK (t):
    r'ROLLBACK'
    return t
def t_RIGHT (t):
    r'RIGHT'
    return t 


# S
def t_SECURITY(t):
    r'SECURITY'
    return t
def t_SECTION(t):
    r'SECTION'
    return t
def t_SOURCE_COMPUTER(t):
    r'SOURCE-COMPUTER'
    return t
def t_SECURE (t):
    r'SECURE'
    return t
def t_SAME (t):
    r'SAME'
    return t
def t_SCREEN (t):
    r'SCREEN'
    return t
def t_SEARCH (t):
    r'SEARCH'
    return t
def t_SEGMENT (t):
    r'SEGMENT'
    return t
def t_SELECT (t):
    r'SELECT'
    return t
def t_SEND (t):
    r'SEND'
    return t
def t_SENTENCE (t):
    r'SENTENCE'
    return t
def t_SEPARATE (t):
    r'SEPARATE'
    return t
def t_SEQUENTIAL (t):
    r'SEQUENTIAL'
    return t
def t_SEQUENCE (t):
    r'SEQUENCE'
    return t
def t_SET (t):
    r'SET'
    return t
def t_SIGN (t):
    r'SIGN'
    return t
def t_SIZE (t):
    r'SIZE'
    return t
def t_SORT (t):
    r'SORT'
    return t
def t_SOURCE (t):
    r'SOURCE'
    return t
def t_SPACE(t):
    r'SPACE'
    return t
def t_SPACES (t):
    r'SPACES'
    return t
def t_STANDARD (t):
    r'STANDARD'
    return t
def t_START (t):
    r'START'
    return t
def t_STATUS (t):
    r'STATUS'
    return t
def t_STOP (t):
    r'STOP'
    return t
def t_SUBTRACT (t):
    r'SUBTRACT'
    return t
def t_SUPPRESS (t):
    r'SUPPRESS'
    return t
def t_SYMBOLIC (t):
    r'SYMBOLIC'
    return t
def t_SYNC (t):
    r'SYNC'
    return t
def t_SYCHRONIZED (t):
    r'SYCHRONIZED'
    return t


# T
def t_TAB (t):
    r'TAB'
    return t
def t_TALLYNG (t):
    r'TALLYNG'
    return t
def t_TAPE (t):
    r'TAPE'
    return t
def t_TERMINAL (t):
    r'TERMINAL'
    return t
def t_TERMINATE (t):
    r'TERMINATE'
    return t
def t_TEST (t):
    r'TEST'
    return t
def t_TEXT (t):
    r'TEXT'
    return t
def t_THAN (t):
    r'THAN'
    return t
def t_THEN (t):
    r'THEN'
    return t
def t_THROUGH (t):
    r'THROUGH'
    return t
def t_THRU (t):
    r'THRU'
    return t
def t_TIME (t):
    r'TIME'
    return t
def t_TIMES (t):
    r'TIMES'
    return t
def t_TO (t):
    r'TO'
    return t
def t_TOP (t):
    r'TOP'
    return t
def t_TRAILING (t):
    r'TRAILING'
    return t
def t_TRUE (t):
    r'TRUE'
    return t   


# U
def t_UNDERLINE (t):
    r'UNDERLINE'
    return t
def t_UNDERLINED (t):
    r'UNDERLINED'
    return t
def t_UNIT (t):
    r'UNIT'
    return t
def t_UNLOCK (t):
    r'UNLOCK'
    return t
def t_UNSTRING (t):
    r'UNSTRING'
    return t
def t_UNTIL (t):
    r'UNTIL'
    return t
def t_UP (t):
    r'UP'
    return t
def t_UPDATE (t):
    r'UPDATE'
    return t
def t_UPDATES (t):
    r'UPDATES'
    return t
def t_UPON (t):
    r'UPON'
    return t
def t_USAGE (t):
    r'USAGE'
    return t
def t_USE (t):
    r'USE'
    return t
def t_USING (t):
    r'USING'
    return t


# V
def t_VALUE (t):
    r'VALUE'
    return t
def t_VALUES (t):
    r'VALUES'
    return t
def t_VARYING (t):
    r'VARYING'
    return t


# W
def t_WORKING_STORAGE(t):
    r'WORKING-STORAGE'
    return t
def t_WITH (t):
    r'WITH'
    return t
def t_WHEN (t):
    r'WHEN'
    return t
def t_WORDS (t):
    r'WORDS'
    return t
def t_WRITE (t):
    r'WRITE'
    return t
def t_WRITERS (t):
    r'WRITERS'
    return t


# Z
def t_ZERO (t):
    r'ZERO'
    return t
def t_ZEROS (t):
    r'ZEROS'
    return t
def t_ZEROES (t):
    r'ZEROES'
    return t

#Expresiones regulares para los principales simbolos 
t_SUMA      = r'\+'
t_RESTA     = r'-'
t_MULTIPLICACION = r'\*'
t_OP_DIVISION  = r'/'
t_IGUAL     = r'='
t_DISTINTO  = r'!'
t_MENOR_QUE  = r'<'
t_MAYOR_QUE  = r'>'
t_PUNTOYCOMA = r';'
t_COMA       = r','
t_PARENT_IZQ = r'\('
t_PARENT_DER = r'\)'
t_CORCHETE_IZQ = r'\['
t_CORCHETE_DER = r'\]'
t_LLAVE_IZQ    = r'{'
t_LLAVE_DER    = r'}'
t_DOS_PUNTOS   = r':'
t_ET        = r'\&'
t_HASHTAG   = r'\#'
t_PUNTO       = r'\.'
t_COMILLAS    = r'\"'
t_APOSTROFE = r'\''
t_RESIDUO   = r'%'
t_IGUALDAD_ESTRICTA = r'==='
t_DESIGUALDAD_ESTRICTA = r'!=='
t_AND_LOGICO = r'&&'
t_OR_LOGICO  = r'\|\|'
t_NOT_LOGICO = r'!'
t_ASIGNACION_MULTI     = r'\*='
t_ASIGNACION_DIVISION  = r'/='
t_ASIGNACION_RESIDUO   = r'%='
t_ASIGNACION_SUMA      = r'\+='
t_ASIGNACION_RESTA     = r'-='

#Expresiones regulares simbolos de operadores condicionales

def t_MENOR_IGUAL(t):
    r'<='
    return t

def t_MAYOR_IGUAL(t):
    r'>='
    return t

def t_DESIGUAL(t):
    r'!='
    return t

def t_ES_IGUAL(t):
    r'=='
    return t
#expresiones regulares para otros simbolos de operadores
def t_DECREMENTO(t):
    r'--'
    return t

def t_INCREMENTO(t):
    r'\+\+'
    return t

def t_EXPONENTE(t):
    r'\*\*'
    return t

#Comentarios
#Opcional, por que la documentacion decia que los comentarios inician en la celda 07 siempre osea al inicio de cada linea
def t_COMMENTS(t):
    r'^\*.*?\n' 
    t.lexer.lineno += t.value.count('\n')
"""
def t_COMMENTS_C99(t):
    r'(\/\/|\#)(.)*?\n'
    t.lexer.lineno += 1

def t_IDENTIFICADOR(t):
    r'\$\w+(\d\w)*'
    return t"""
#ER para validar numeros enteros y decimales
def t_NUMERO(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value)
    return t
#RE para buscar variables
def t_VARIABLE(t):
    r'[a-zA-Z]+[a-zA-Z0-9\-]*[a-zA-Z0-9]+'
    #r'\w+[a-zA-Z0-9\-]*'
    return t
#RE para buscar cadenas
def t_STRING(t):
    r'(("[^"]*")|(\'[^\']*\'))'
    return t

#instanciamos un objeto lex
lexer = lex.lex()
script = 'ifelse.cbl' #cargamos el archivo
scriptfile = open(script, 'r')  #abrimos el archivo
scriptdata = scriptfile.read() #se lee el archivo
lexer.input(scriptdata) #entrada del texto a analizar

tbl = dict()
linea=[]
tok_val=[]
lexema=[]
cont=[]
#banner de color
print (chr(27)+"[0;36m"+"INICIO DEL ANÁLISIS LÉXICO"+chr(27)+"[0m")
i = 1
while True:
    #analiza los patrones
    tok = lexer.token()
    if not tok:
        break
    linea.append(str(tok.lineno))
    tok_val.append(str(tok.type))
    lexema.append(str(tok.value))
    print ("\t"+str(i)+" - "+"Linea: "+str(tok.lineno)+"\t"+str(tok.type)+"  -->  "+str(tok.value))
    cont.append(i)
    i += 1
tbl['linea'] = linea
tbl['Tipo de token'] = tok_val
tbl['Valor-lexema'] = lexema
df = pd.DataFrame(tbl)
#print(df.to_markdown(index=False)) 
print("================IMPRESION EN TABLA CON FORMATO==================")
print(tabulate(df, headers = 'keys', tablefmt = 'psql', showindex=cont))

print (chr(27)+"[0;36m"+"FIN DEL ANÁLISIS LÉXICO"+chr(27)+"[0m")
print("ANALIZADOR LEXICO PARA COBOL REALIZADO POR:"+"\n"
        +"--LUIS SANTIAGO NOH CAHUM--19070049--"+"\n"
        +"--JESUS ISRAEL GAMBOA AKE--19070041--"+"\n"
        +"--MARICELA DEL ROSARIO PUC OY--19070051--"+"\n"
        +"--ROGER DAVID ABAN KU--19070025--")


#la x puede ser un identificador una X o mas X o un 9 o mas 9

