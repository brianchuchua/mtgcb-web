import ToggleableButton, { ToggleButtonProps } from './ToggleableButton';
import { BlackMana } from '../../../../components/symbols/mana';

const BlackManaButton: React.FC<ToggleButtonProps> = ({ size = 'small', toggled, handleClick }) => (
  <ToggleableButton size={size} toggled={toggled} handleClick={handleClick}>
    <BlackMana size={2} />
  </ToggleableButton>
);

export default BlackManaButton;
