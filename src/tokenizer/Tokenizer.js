import Visitor from '../visitor/Visitor.js';
import { Rango } from '../visitor/CST.js';
import { generateCaracteres } from './utils.js';

export default class Tokenizer extends Visitor {
    visitProducciones(node) {
        return node.expr.accept(this);
    }
    visitOpciones(node) {
        return node.exprs
            .map((expr) => expr.accept(this))
            .filter((str) => str)
            .join('\n');
    }
    visitUnion(node) {
        return node.exprs
            .map((expr) => expr.accept(this))
            .filter((str) => str)
            .join('\n');
    }
    visitExpresion(node) {
        return node.expr.accept(this);
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
        return `
    i = cursor
    ${generateCaracteres(node.chars.filter((node) => typeof node === 'string'))}
    ${node.chars
                .filter((node) => node instanceof Rango)
                .map((range) => range.accept(this))
                .join('\n')}
        `;
    }
    visitRango(node) {
        return `
    if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    }
    visitIdentificador(node) {
        return '';
    }
    visitPunto(node) {
        return '';
    }
    visitFin(node) {
        return '';
    }
}
