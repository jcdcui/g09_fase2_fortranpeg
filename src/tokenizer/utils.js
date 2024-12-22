import Tokenizer from './Tokenizer.js';

export async function generateTokenizer(grammar) {
    const tokenizer = new Tokenizer();
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

export function generateCaracteres(chars) {
    if (chars.length === 0) return '';
    return `
    if (findloc([${chars
        .map((char) => `"${char}"`)
        .join(', ')}], input(i:i), 1) > 0) then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
    `;
}
