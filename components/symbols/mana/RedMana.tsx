import ManaSymbol, { ManaSymbolProps } from './ManaSymbol';

const RedMana: React.FC<ManaSymbolProps> = ({ size = 3 }) => <ManaSymbol size={size} color="r" />;

export default RedMana;
