import Paper from '@material-ui/core/Paper';
import ToggleButton from '@material-ui/lab/ToggleButton';
import ToggleButtonGroup from '@material-ui/lab/ToggleButtonGroup';
import styled from 'styled-components';
import { CardEditingModes } from '../../features/edit-cards/editCardsSlice';

interface CardEditingModeSelectorProps {
  cardEditingMode: CardEditingModes;
  setCardEditingMode: (editingMode: CardEditingModes) => void;
}

const CardEditingModeSelector: React.FC<CardEditingModeSelectorProps> = ({ cardEditingMode, setCardEditingMode }) => {
  const updateCardEditingMode = (editingModeValue) => {
    setCardEditingMode(editingModeValue);
  };

  const handleEditingModeChange = (event: React.MouseEvent<HTMLElement>, newEditingMode: CardEditingModes | null) => {
    if (newEditingMode != null) {
      updateCardEditingMode(newEditingMode);
    }
  };

  return (
    <>
      <ToggleGroupWrapper>
        <StyledToggleButtonGroup value={cardEditingMode} onChange={handleEditingModeChange} exclusive aria-label="select card editing mode">
          <StyledToggleButton value="increment" aria-label="increment">
            Increment
          </StyledToggleButton>
          <StyledToggleButton value="set" aria-label="set">
            Set
          </StyledToggleButton>
        </StyledToggleButtonGroup>
      </ToggleGroupWrapper>
    </>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '10px', marginTop: '0px' }));
const StyledToggleButton = styled(ToggleButton)(() => ({ alignItems: 'end', justifyContent: 'center', width: '100%' }));
const StyledToggleButtonGroup = styled(ToggleButtonGroup)(() => ({ width: '100%' }));

export default CardEditingModeSelector;
