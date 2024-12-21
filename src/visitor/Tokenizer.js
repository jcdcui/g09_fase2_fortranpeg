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
${final} 
        `
        }else{
           
       let unirExp = `
        bufferConc = bufferConc // lexeme  
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
   ${final}
    cursor = carro
                    `;
                    
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
    end if
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
    end if
                    
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


    
    visitClase(node) {
        const isCase = node.isCase !== null;
        return `
        i = cursor
        ${isCase 
            ? this.generateCaracteresCaseInsensitive(
                node.chars.filter((node) => typeof node === 'string')
            )
            : this.generateCaracteres(
                node.chars.filter((node) => typeof node === 'string')
            )}
        ${node.chars
            .filter((node) => node instanceof Rango)
            .map((range) => isCase ? this.visitRangoCaseInsensitive(range) : this.visitRango(range))
            .join('\n')}
        `;
    }
    
    visitRango(node) {
        return `
        if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then
            lexeme = input(cursor:i)
            cursor = i + 1
        `;
    }

    visitIdentificador(node){
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
        return `
        if (findloc([${chars.map((char) => `"${char}"`).join(', ')}], input(i:i), 1) > 0) then
            lexeme = input(cursor:i)
            cursor = i + 1
        `;
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
