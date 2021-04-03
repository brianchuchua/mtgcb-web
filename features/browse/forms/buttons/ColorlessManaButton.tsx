import ToggleableButton from './ToggleableButton';
import { ColorlessMana } from '../../../../components/symbols/mana';

const ColorlessManaButton = ({ size = 'small', toggled, handleClick }) => (
  <ToggleableButton size={size} toggled={toggled} handleClick={handleClick}>
    <ColorlessMana size={2} />
  </ToggleableButton>
);

export default ColorlessManaButton;
