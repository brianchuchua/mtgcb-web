import FormLabel from '@material-ui/core/FormLabel';
import Paper from '@material-ui/core/Paper';
import ToggleButton from '@material-ui/lab/ToggleButton';
import ToggleButtonGroup from '@material-ui/lab/ToggleButtonGroup';
import styled from 'styled-components';

interface QuantitySelectorProps {
  quantity: string;
  setQuantity: (quantity: string) => void;
  label: string;
}

const QuantitySelector: React.FC<QuantitySelectorProps> = ({ quantity, setQuantity, label }) => {
  const updateQuantity = (quantityValue) => {
    setQuantity(quantityValue);
  };

  const handleQuantityChange = (event: React.MouseEvent<HTMLElement>, newQuantity: string | null) => {
    if (newQuantity != null) {
      updateQuantity(newQuantity);
    }
  };

  return (
    <ToggleGroupWrapper>
      <StyledFormLabel>{label}</StyledFormLabel>
      <StyledToggleButtonGroup value={quantity} onChange={handleQuantityChange} exclusive aria-label={`select quantity (${label}) to view`}>
        <StyledToggleButton value="all" aria-label="all" size="small">
          All
        </StyledToggleButton>
        <StyledToggleButton value="5x+" aria-label="5x+" size="small">
          5x+
        </StyledToggleButton>
        <StyledToggleButton value="4x" aria-label="4x" size="small">
          4x
        </StyledToggleButton>
        <StyledToggleButton value="3x" aria-label="3x" size="small">
          3x
        </StyledToggleButton>
        <StyledToggleButton value="2x" aria-label="2x" size="small">
          2x
        </StyledToggleButton>
        <StyledToggleButton value="1x" aria-label="1x" size="small">
          1x
        </StyledToggleButton>
        <StyledToggleButton value="0x" aria-label="0x" size="small">
          0x
        </StyledToggleButton>
      </StyledToggleButtonGroup>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '10px', marginTop: '0px', marginBottom: '10px' }));
const StyledToggleButton = styled(ToggleButton)(() => ({ alignItems: 'end', justifyContent: 'center', width: '100%' }));
const StyledToggleButtonGroup = styled(ToggleButtonGroup)(() => ({ width: '100%' }));
const StyledFormLabel = styled(FormLabel)(() => ({ paddingBottom: '5px', display: 'block', fontSize: '0.8rem' }));

export default QuantitySelector;
