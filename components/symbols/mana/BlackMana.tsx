import ManaSymbol, { ManaSymbolProps } from './ManaSymbol';

const BlackMana: React.FC<ManaSymbolProps> = ({ size = 3 }) => <ManaSymbol size={size} color="b" />;

export default BlackMana;
