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
                    return `(input(i:i) == ${this.toAsciiString(char,0)} .or. input(i:i) == ${this.toAsciiString(char,1)})`;
                } else {
                    return `input(i:i) == ${this.toAsciiString(char,2)}`;
                }
            });
    
        const rangeConditions = node.chars
            .filter((char) => char instanceof Rango)
            .map(range => {
                if (isCase) {
                    return `((input(i:i) >= ${this.toAsciiString(range.bottom,0)} .and. input(i:i) <= ${this.toAsciiString(range.top,0)}) .or. &
                            (input(i:i) >= ${this.toAsciiString(range.bottom,1)} .and. input(i:i) <= ${this.toAsciiString(range.bottom,1)}))`;
                } else {
                    return `(input(i:i) >= ${this.toAsciiString(range.bottom,2)} .and. input(i:i) <= ${this.toAsciiString(range.bottom,2)})`;
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

toAsciiString(char,num) {
    console.log(char)
    const charMap = {
        '\\t': 9,  
        '\\n': 10, 
        '\\r': 13, 
        ' ': 32   
    };

    
    if (char in charMap) {
        return `char(${charMap[char]})`;
    } else {
        if (char >= 'A' && char <= 'Z' && num == 1) {
            char = String.fromCharCode(char.charCodeAt(0) + 32);
        }else if(char >= 'a' && char <= 'a' && num == 0){
            char = String.fromCharCode(char.charCodeAt(0) - 32);
        }
            
        return `char(${char.charCodeAt(0)})`;
    }

}

}