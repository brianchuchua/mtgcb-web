import { WhiteMana } from '../../../../components/symbols/mana';
import ToggleableButton, { ToggleButtonProps } from './ToggleableButton';

const WhiteManaButton: React.FC<ToggleButtonProps> = ({ size = 'small', toggled, handleClick }) => (
  <ToggleableButton size={size} toggled={toggled} handleClick={handleClick}>
    <WhiteMana size={2} />
  </ToggleableButton>
);

export default WhiteManaButton;
