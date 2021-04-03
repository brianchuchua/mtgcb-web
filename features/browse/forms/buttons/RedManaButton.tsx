import ToggleableButton from './ToggleableButton';
import { RedMana } from '../../../../components/symbols/mana';

const RedManaButton = ({ size = 'small', toggled, handleClick }) => (
  <ToggleableButton size={size} toggled={toggled} handleClick={handleClick}>
    <RedMana size={2} />
  </ToggleableButton>
);

export default RedManaButton;
