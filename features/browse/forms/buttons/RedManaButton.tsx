import ToggleableButton, { ToggleButtonProps } from './ToggleableButton';
import { RedMana } from '../../../../components/symbols/mana';

const RedManaButton: React.FC<ToggleButtonProps> = ({ size = 'small', toggled, handleClick }) => (
  <ToggleableButton size={size} toggled={toggled} handleClick={handleClick}>
    <RedMana size={2} />
  </ToggleableButton>
);

export default RedManaButton;
