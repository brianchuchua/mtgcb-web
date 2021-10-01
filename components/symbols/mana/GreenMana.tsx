import ManaSymbol, { ManaSymbolProps } from './ManaSymbol';

const GreenMana: React.FC<ManaSymbolProps> = ({ size = 3 }) => <ManaSymbol size={size} symbol="g" />;

export default GreenMana;
