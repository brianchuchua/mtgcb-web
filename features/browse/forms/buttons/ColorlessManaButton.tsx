import ToggleableButton, { ToggleButtonProps } from './ToggleableButton';
import { ColorlessMana } from '../../../../components/symbols/mana';

const ColorlessManaButton: React.FC<ToggleButtonProps> = ({ size = 'small', toggled, handleClick }) => (
  <ToggleableButton size={size} toggled={toggled} handleClick={handleClick}>
    <ColorlessMana size={2} />
  </ToggleableButton>
);

export default ColorlessManaButton;
