import ManaSymbol, { ManaSymbolProps } from './ManaSymbol';

const GreenMana: React.FC<ManaSymbolProps> = ({ size = 3 }) => <ManaSymbol size={size} color="g" />;

export default GreenMana;
