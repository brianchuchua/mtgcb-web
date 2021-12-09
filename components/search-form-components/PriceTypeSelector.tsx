import Paper from '@material-ui/core/Paper';
import ToggleButton from '@material-ui/lab/ToggleButton';
import ToggleButtonGroup from '@material-ui/lab/ToggleButtonGroup';
import styled from 'styled-components';

interface PriceTypeSelectorProps {
  priceType: string;
  setPriceType: (priceType: string) => void;
}

const PriceTypeSelector: React.FC<PriceTypeSelectorProps> = ({ priceType, setPriceType }) => {
  const updatePriceType = (priceTypeValue) => {
    setPriceType(priceTypeValue);
  };

  const handlePriceTypeChange = (event: React.MouseEvent<HTMLElement>, newPriceType: string | null) => {
    if (newPriceType != null) {
      updatePriceType(newPriceType);
    }
  };

  return (
    <ToggleGroupWrapper>
      <StyledToggleButtonGroup value={priceType} onChange={handlePriceTypeChange} exclusive aria-label="select prices to view">
        <StyledToggleButton value="market" aria-label="market">
          Market
        </StyledToggleButton>
        <StyledToggleButton value="low" aria-label="low">
          Low
        </StyledToggleButton>
        <StyledToggleButton value="average" aria-label="average">
          Avg
        </StyledToggleButton>
        <StyledToggleButton value="high" aria-label="high">
          High
        </StyledToggleButton>
      </StyledToggleButtonGroup>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '10px', marginTop: '0px', marginBottom: '15px' }));
const StyledToggleButton = styled(ToggleButton)(() => ({ alignItems: 'end', justifyContent: 'center', width: '100%' }));
const StyledToggleButtonGroup = styled(ToggleButtonGroup)(() => ({ width: '100%' }));

export default PriceTypeSelector;
