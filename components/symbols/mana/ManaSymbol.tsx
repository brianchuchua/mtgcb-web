const ManaSymbol: React.FC<ManaSymbolProps> = ({ size = 3, color = 'c' }) => <i className={`ms ms-${color} ms-cost ms-${size}x`} />;
export interface ManaSymbolProps {
  size: number;
  color?: 'w' | 'u' | 'b' | 'r' | 'g' | 'c';
}

export default ManaSymbol;
