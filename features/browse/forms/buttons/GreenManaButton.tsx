import ToggleableButton from './ToggleableButton';
import { GreenMana } from '../../../../components/symbols/mana';

const GreenManaButton = ({ size = 'small', toggled, handleClick }) => (
  <ToggleableButton size={size} toggled={toggled} handleClick={handleClick}>
    <GreenMana size={2} />
  </ToggleableButton>
);

export default GreenManaButton;
