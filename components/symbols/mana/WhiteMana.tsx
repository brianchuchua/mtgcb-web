import ManaSymbol, { ManaSymbolProps } from './ManaSymbol';

const WhiteMana: React.FC<ManaSymbolProps> = ({ size = 3 }) => <ManaSymbol size={size} color="w" />;

export default WhiteMana;
