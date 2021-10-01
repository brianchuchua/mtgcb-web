import ManaSymbol, { ManaSymbolProps } from './ManaSymbol';

const WhiteMana: React.FC<ManaSymbolProps> = ({ size = 3 }) => <ManaSymbol size={size} symbol="w" />;

export default WhiteMana;
