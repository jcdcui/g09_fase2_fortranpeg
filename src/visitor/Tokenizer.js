import Visitor from './Visitor.js';
import { Opciones, Rango, Union,Clase, Cadena } from './CST.js';
import { Identificador } from './CST.js';

let concNum = 0;
let cuantificar  = true;
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
    integer :: i,j
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

visitUnion(node){

const nullQtyCount = node.exprs.filter(expr => expr.qty).length;
let final = ""
 
        for (let i = 0; i < node.exprs.length-nullQtyCount; i++) 
                { final += 'end if\n';}


        let unirExp = `\nbufferConc = bufferConc // lexeme\n`;
        let initConcat = `
bufferConc = ""
carro = cursor`
        let concNodos = '';
        
        for (let i = 0; i < node.exprs.length; i++) {
            let previousExpr = node.exprs[i - 1];
            let currentExpr = node.exprs[i];
            let nextExpr = node.exprs[i + 1];
            console.log("START"+concNum)
            if ((currentExpr.expr instanceof Clase || currentExpr.expr instanceof Cadena) &&
                (previousExpr && (previousExpr.expr instanceof Clase || previousExpr.expr instanceof Cadena)) &&
                (nextExpr && (nextExpr.expr instanceof Clase || nextExpr.expr instanceof Cadena))) {
                    if(currentExpr.qty){
                        concNodos +='\nend if\n'+ currentExpr.accept(this) + unirExp+'\n ! Instancia de Clase o Cadena, con anterior y siguiente también siendo de Clase o Cadena1';
                    }else{
                concNum+=1
                concNodos += currentExpr.accept(this) + unirExp+'\n ! Instancia de Clase o Cadena, con anterior y siguiente también siendo de Clase o Cadena1';
                    }
            } else if ((currentExpr.expr instanceof Clase || currentExpr.expr instanceof Cadena) &&
                       (!nextExpr || !(nextExpr.expr instanceof Clase || nextExpr.expr instanceof Cadena)) &&
                       (!previousExpr || !(previousExpr.expr instanceof Clase || previousExpr.expr instanceof Cadena))) {

                        concNodos += currentExpr.accept(this) + '\nreturn \nend if'+'\n ! Instancia de Clase o Cadena, con anterior y siguiente siendo otra cosa */2';
                            
            } else if ((currentExpr.expr instanceof Clase || currentExpr.expr instanceof Cadena) &&
                       (!nextExpr || !(nextExpr.expr instanceof Clase || nextExpr.expr instanceof Cadena)) &&
                       (previousExpr && (previousExpr.expr instanceof Clase || previousExpr.expr instanceof Cadena))) {
                        if(!currentExpr.qty){
                            concNum+=1
                            concNodos += currentExpr.accept(this) +this.getEndConcat(concNum)+ '\n ! Instancia de Clase o Cadena, con anterior siendo de Clase o Cadena y siguiente siendo otra cosa */3';
                        }else{
                        concNodos += currentExpr.accept(this) +'\nend if\n'+this.getEndConcat(concNum)+ '\n ! Instancia de Clase o Cadena, con anterior siendo de Clase o Cadena y siguiente siendo otra cosa */3';
                    }
                    concNum = 0;
            } else if ((currentExpr.expr instanceof Clase || currentExpr.expr instanceof Cadena) &&
                       (nextExpr && (nextExpr.expr instanceof Clase || nextExpr.expr instanceof Cadena)) &&
                       (!previousExpr || !(previousExpr.expr instanceof Clase || previousExpr.expr instanceof Cadena))) {
                        if(currentExpr.qty){
                            concNodos += initConcat+currentExpr.accept(this) +'\nend if\n'+unirExp+'\n ! Instancia de Clase o Cadena, con anterior siendo otra cosa y siguiente siendo de Clase o Cadena */4';
                        }else{
                        concNum+=1
                        concNodos += initConcat+currentExpr.accept(this) +unirExp+'\n ! Instancia de Clase o Cadena, con anterior siendo otra cosa y siguiente siendo de Clase o Cadena */4';
                        }
            } else if (currentExpr.expr instanceof Opciones){
                cuantificar = false;
                concNodos +="\n !EMPIEZA OP\n "+currentExpr.accept(this)+"\n !Termina OP\n";
                
            } else {
                concNodos +=' ! Otra cosa */';
            }
            console.log("END"+concNum)
        }
        
        
        

        
return concNodos
}




    /*visitUnion(node) {

const nullExprCount = node.exprs.filter(expr => expr === null).length;

console.log("T: "+node.exprs.length);
const nullQtyCount = node.exprs.filter(expr => expr.qty).length;
console.log("Q: "+nullQtyCount);

       let final = ""
 
        for (let i = 0; i < node.exprs.length-nullQtyCount; i++) 
                { final += 'end if\n';}

        if(node.exprs.length == 1){
            console.log("__________");
            let concNodos = node.exprs
            .map((expr) => expr.accept(this))
            .filter((str) => str)
            .join('\n');;

            if (!/[a-zA-Z]/.test(concNodos)){
                return ''
            }

            if(nullQtyCount>0){
                console.log("nullQtyCount");
            return `
            ${this.removeLastEndIf(concNodos)}7
            return
            end if 
                        `
            }else{
            return `
${concNodos}8
return
end if 
        `
            }

        }else{
           
       let unirExp = `\nbufferConc = bufferConc // lexeme`;
       let concNodos = node.exprs
       .map((expr) => expr.accept(this))
       .filter((str) => str)
       .join(unirExp);
    let concatenacion = ""
       if (!/[a-zA-Z]/.test(concNodos)){
            return ''
       }
       concatenacion = 
`
bufferConc1 = ""
carro1 = cursor
    ${concNodos}6   
bufferConc = bufferConc // lexeme
if (len(bufferConc) > 0 .and. index(bufferConc, "ERROR") == 0) then
    
    if (allocated(lexeme)) deallocate(lexeme)
    allocate(character(len=len(bufferConc)) :: lexeme)
    lexeme = bufferConc
    return
end if

${final} 
cursor = carro2
                    `;
                    
                    return concatenacion;
        }

        
      

    }
*/
    visitExpresion(node) {
        let generatedCode = node.expr.accept(this);

        if (node.qty && !(node.expr instanceof Opciones)) {
            
            switch (node.qty) {
                case '*': // Cero o más
                    return `
! Inicializar variables
buffer = ""
count = 0
do j = cursor, len(input)
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
        lexeme = buffer`;
                case '+': // Uno o más
                    return `
! Inicializar variables
buffer = ""
count = 0
do j = cursor, len(input)
    ${generatedCode}
    buffer = buffer // lexeme
else
    exit
end if
end do

if (len(buffer) == 0) then
    lexeme = "ERROR"
else
    if (allocated(lexeme)) deallocate(lexeme)
        allocate(character(len=len(buffer)) :: lexeme)
        lexeme = buffer`;
                case '?': // Cero o uno
                    return `
!! Inicializar variables
buffer = ""
count = 0
do j = 1, 1
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
        lexeme = buffer`;
                default:
                    return `
                    ${generatedCode}`;
            }
        }

        return `${generatedCode}`;
    }

    visitCadena(node) {
        if (node.isCase !== null) {
            return `
if (to_lower(input(cursor:cursor + ${node.val.length - 1})) == to_lower("${node.val}")) then
    if (allocated(lexeme)) deallocate(lexeme)
    allocate(character(len=${node.val.length}) :: lexeme)
    lexeme = input(cursor:cursor + ${node.val.length - 1})
    cursor = cursor + ${node.val.length}`;
        } else {
            return `
if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then
    if (allocated(lexeme)) deallocate(lexeme)
    allocate(character(len=${node.val.length}) :: lexeme)
    lexeme = input(cursor:cursor + ${node.val.length - 1})
    cursor = cursor + ${node.val.length}`;
        }
    }

    visitRango(node) {
        return '';
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
                            (input(i:i) >= ${this.toAsciiString(range.bottom,1)} .and. input(i:i) <= ${this.toAsciiString(range.top,1)}))`;
                } else {
                    return `(input(i:i) >= ${this.toAsciiString(range.bottom,2)} .and. input(i:i) <= ${this.toAsciiString(range.top,2)})`;
                }
            });
    
        conditions = charConditions.concat(rangeConditions).join(' .or. &\n');
    
        return `
i = cursor
if (${conditions}) then
    lexeme = input(cursor:i)
    cursor = i + 1`;
    }
    
    
    visitIdentificador(node){
        return '';
    }
    
    visitPunto(node) {
        return '';
    }
    visitFin(node) {
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
        }else if(char >= 'a' && char <= 'z' && num == 0){
            char = String.fromCharCode(char.charCodeAt(0) - 32);
        }
            
        return `char(${char.charCodeAt(0)})`;
    }

}
 removeLastEndIf(str) {
    const searchString = "end if";
    const lastIndex = str.lastIndexOf(searchString);
    console.log(searchString)
    if (lastIndex === -1) {
     
        return str;
    }

    const beforeLastOccurrence = str.substring(0, lastIndex);
    const afterLastOccurrence = str.substring(lastIndex + searchString.length);

    return beforeLastOccurrence + afterLastOccurrence;
}

 getEndIfs(num){
    let endIfs = ""
    for (let i = 0; i < num; i++) 
        { endIfs += '\nend if';}
    return endIfs
 }

 getEndConcat(num){
    console.log("NUM"+num)
    let endConc = `
    bufferConc = bufferConc // lexeme
    if (len(bufferConc) > 0 .and. index(bufferConc, "ERROR") == 0) then
        
        if (allocated(lexeme)) deallocate(lexeme)
        allocate(character(len=len(bufferConc)) :: lexeme)
        lexeme = bufferConc
        return
    end if
    ${this.getEndIfs(num)}
    cursor = carro`

    return endConc
 }

 removeLastEndIf(str) {
    const searchString = "end if";
    const lastIndex = str.lastIndexOf(searchString);
    console.log(searchString)
    if (lastIndex === -1) {
     
        return str;
    }

    const beforeLastOccurrence = str.substring(0, lastIndex);
    const afterLastOccurrence = str.substring(lastIndex + searchString.length);

    return beforeLastOccurrence + afterLastOccurrence;
}

}