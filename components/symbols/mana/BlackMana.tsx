import ManaSymbol, { ManaSymbolProps } from './ManaSymbol';

const BlackMana: React.FC<ManaSymbolProps> = ({ size = 3 }) => <ManaSymbol size={size} symbol="b" />;

export default BlackMana;
