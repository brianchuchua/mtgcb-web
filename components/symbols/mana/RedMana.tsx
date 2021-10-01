import ManaSymbol, { ManaSymbolProps } from './ManaSymbol';

const RedMana: React.FC<ManaSymbolProps> = ({ size = 3 }) => <ManaSymbol size={size} symbol="r" />;

export default RedMana;
