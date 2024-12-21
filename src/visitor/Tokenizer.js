import Visitor from './Visitor.js';
import { Rango } from './CST.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        return `
module tokenizer
implicit none

contains
function nextSym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme
    integer :: i
    character(len=:), allocatable :: buffer
    character(len=:), allocatable :: bufferConc
    integer :: count
    integer :: carro

    if (cursor > len(input)) then
        allocate(character(len=3) :: lexeme)
        lexeme = "EOF"
        return
    end if

    ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

    print *, "error lexico en col ", cursor, ', "' // input(cursor:cursor) // '"'
    lexeme = "ERROR"
end function nextSym


function to_lower(str) result(lower_str)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower_str
    integer :: i

    do i = 1, len(str)
        if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
            lower_str(i:i) = achar(iachar(str(i:i)) + 32)
        else
            lower_str(i:i) = str(i:i)
        end if
    end do
end function to_lower
function char_to_ascii(character) result(ascii_code)
    character(len=1), intent(in) :: character
    integer :: ascii_code

    ascii_code = iachar(character)
end function char_to_ascii

end module tokenizer

        `;
    }

    visitProducciones(node) {
        return node.expr.accept(this);
    }

    visitOpciones(node) {

        return node.exprs.map((node) => node.accept(this)).join('\n');
    }

    visitUnion(node) {
       
const nullExprCount = node.exprs.filter(expr => expr === null).length;

console.log(nullExprCount);
const nullQtyCount = node.exprs.filter(expr => expr.qty).length;
console.log(nullQtyCount);

        let final = ""
 
        for (let i = 0; i < node.exprs.length-nullQtyCount; i++) 
                { final += 'end if\n';}
        
        if(node.exprs.length == 1){
            let concNodos = node.exprs.map((node) => node.accept(this)).join('');
          

            return `
${concNodos}
return
end if
        `
        }else{
           
       let unirExp = `\nend if \nbufferConc = bufferConc // lexeme  
                    `;
       let concNodos = node.exprs.map((node) => node.accept(this)).join(unirExp);
       let concatenacion = `
bufferConc = ""
carro = cursor
    ${concNodos}
        bufferConc = bufferConc // lexeme 
        if (allocated(lexeme)) deallocate(lexeme)
        allocate(character(len=len(bufferConc)) :: lexeme)
        lexeme = bufferConc
        return         
   end if
    cursor = carro
                    `;
                    variables[actualID].push(concNodos)
                    return concatenacion;
        }

        
      

    }

    visitExpresion(node) {
        let generatedCode = node.expr.accept(this);

        if (node.qty) {
            switch (node.qty) {
                case '*': // Cero o más
                    return `
! Inicializar variables
buffer = ""
count = 0
do
    if (cursor > len(input)) exit
    ${generatedCode}
    count = 1
    buffer = buffer // lexeme
else
    exit
end if
end do

if (len(buffer) == 0) then
    lexeme = ""
else
if (allocated(lexeme)) deallocate(lexeme)
    allocate(character(len=len(buffer)) :: lexeme)
    lexeme = buffer

 
                    `;
                case '+': // Uno o más
                    return `
! Inicializar variables
buffer = ""
count = 0
do
    if (cursor > len(input)) exit
    ${generatedCode}
    buffer = buffer // lexeme
else
    exit
end if
end do

if (len(buffer) == 0) then
    lexeme = "ERROR"
    print *, "error lexico en col ", cursor, ', "' // input(cursor:cursor) // '"'
    return
else
if (allocated(lexeme)) deallocate(lexeme)
    allocate(character(len=len(buffer)) :: lexeme)
    lexeme = buffer
    end if
                    `;
                case '?': // Cero o uno
                    return `
! Inicializar variables
buffer = ""
count = 0
do
    if (cursor > len(input)) exit
    ${generatedCode}
    count = 1
    if(count > 0) then
        buffer = buffer // lexeme
        exit
    end if
else
    exit
end if
end do

if (len(buffer) == 0) then
    lexeme = ""
else
if (allocated(lexeme)) deallocate(lexeme)
    allocate(character(len=len(buffer)) :: lexeme)
    lexeme = buffer
    
                    
`;
                default:
                    return `
                    ${generatedCode}`;
            }
        }

        return `
${generatedCode}`;
    }

    visitString(node) {
        if (node.isCase !== null) {
            return `
if (to_lower(input(cursor:cursor + ${node.val.length - 1})) == to_lower("${node.val}")) then
    if (allocated(lexeme)) deallocate(lexeme)
    allocate(character(len=${node.val.length}) :: lexeme)
    lexeme = input(cursor:cursor + ${node.val.length - 1})
    cursor = cursor + ${node.val.length}

            `;
        } else {
            return `
if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then
    if (allocated(lexeme)) deallocate(lexeme)
    allocate(character(len=${node.val.length}) :: lexeme)
    lexeme = input(cursor:cursor + ${node.val.length - 1})
    cursor = cursor + ${node.val.length}

            `;
        }
    }

    visitRango(node) {
        return node.accept(this);
    }
    
    visitClase(node) {
        const isCase = node.isCase !== null;
        let conditions = ``;
    
        const charConditions = node.chars
            .filter((char) => typeof char === 'string')
            .map(char => {
                if (isCase) {
                    return `(input(i:i) == to_lower("${char}") .or. input(i:i) == to_lower("${char})")`;
                } else {
                    return `input(i:i) == "${char}"`;
                }
            });
    
        const rangeConditions = node.chars
            .filter((char) => char instanceof Rango)
            .map(range => {
                if (isCase) {
                    return `((input(i:i) >= to_lower("${range.bottom}") .and. input(i:i) <= "${range.top}")`;
                } else {
                    return `(input(i:i) >= "${range.bottom}" .and. input(i:i) <= "${range.top}")`;
                }
            });
    
        conditions = charConditions.concat(rangeConditions).join(' .or. &\n');
    
        return `
            i = cursor
            if (${conditions}) then
                lexeme = input(cursor:i)
                cursor = i + 1
           
        `;
    }
    
    
    

    visitIdentificador(node){
        if (variables.hasOwnProperty(node.id)) {
                return variables[node.id];
            }
        console.log("aun no")
        return '';
    }
    

    // Metodos auxiliares**************************************************************************
    visitRangoCaseInsensitive(node) {
        return `
        if (to_lower(input(i:i)) >= to_lower("${node.bottom}") .and. to_lower(input(i:i)) <= to_lower("${node.top}")) then
            lexeme = input(cursor:i)
            cursor = i + 1
        `;
    }
    
    generateCaracteres(chars) {
        if (chars.length === 0) return '';
        let caracteres = chars.map((char) => `"${char}"`)
        console.log(caracteres)
        let vars = this.formatCharsToAsciiString(caracteres)
        console.log(vars)
        return `
        if (findloc(${vars}, input(i:i), 1) > 0) then
            lexeme = input(cursor:i)
            cursor = i + 1
        `;
    }
// Función para convertir un carácter a su código ASCII
 char_to_ascii(char) {
    return char.charCodeAt(0);
}

// Función principal para convertir un array de caracteres a códigos ASCII y formatearlos
formatCharsToAsciiString(chars) {
    const charMap = {
        '"\\t"': 9,  // Tab
        '"\\n"': 10, // Nueva línea
        '"\\r"': 13, // Retorno de carro
        '" "': 32   // Espacio
    };

    const asciiCodes = chars.map((char) => {
        if (char in charMap) {
            return charMap[char];
        } else {
            // Retirar las comillas dobles del inicio y fin de cada elemento
            const actualChar = char.slice(1, -1);
            return thischar_to_ascii(actualChar);
        }
    });

    const formattedString = asciiCodes.map((code) => `char(${code})`).join(', ');

    return `[${formattedString}]`;
}


    generateCaracteresCaseInsensitive(chars) {
        if (chars.length === 0) return '';
        return `
        if (findloc([${chars.map((char) => `to_lower("${char}")`).join(', ')}], to_lower(input(i:i)), 1) > 0) then
            lexeme = input(cursor:i)
            cursor = i + 1
        `;
    }
    
    
    
    
}
