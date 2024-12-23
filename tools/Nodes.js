const nodes = {
    Producciones: ['id', 'expr', 'alias'],
    Opciones: ['exprs'],
    Union: ['exprs'],
    Expresion: ['expr', 'label', 'qty'],
    Cadena: ['val', 'isCase'],
    Clase: ['chars', 'isCase'],
    Rango: ['bottom', 'top'],
    Identificador: ['id'],
    Punto: [],
    Fin: [],
};

export default nodes;