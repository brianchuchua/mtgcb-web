import { WhiteMana } from '../../../../components/symbols/mana';
import ToggleableButton from './ToggleableButton';

const WhiteManaButton = ({ size = 'small', toggled, handleClick }) => (
  <ToggleableButton size={size} toggled={toggled} handleClick={handleClick}>
    <WhiteMana size={2} />
  </ToggleableButton>
);

export default WhiteManaButton;
