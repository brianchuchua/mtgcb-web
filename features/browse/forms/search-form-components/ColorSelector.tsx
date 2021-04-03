import { useSelector, useDispatch } from 'react-redux';
import styled from 'styled-components';
import Select from '@material-ui/core/Select';
import MenuItem from '@material-ui/core/MenuItem';
import Paper from '@material-ui/core/Paper';
import { RootState } from '../../../../redux/rootReducer';
import { setCardColors, setColorType, ColorTypes } from '../../browseSlice';
import { WhiteManaButton, BlueManaButton, BlackManaButton, RedManaButton, GreenManaButton, ColorlessManaButton } from '../buttons';

const ColorSelector: React.FC = () => {
  const dispatch = useDispatch();
  const { cardColors } = useSelector((state: RootState) => state.browse);

  const updateCardColors = (color: string) => {
    dispatch(setCardColors(color));
  };

  const updateColorType = (type: ColorTypes) => {
    dispatch(setColorType(type));
  };

  const handleManaButtonClick = (color) => {
    updateCardColors(color);
  };

  const handleColorTypeChange = (event: React.ChangeEvent<{ value: ColorTypes }>) => {
    updateColorType(event.target.value);
  };

  return (
    <ColorSelectorWrapper variant="outlined">
      <ColorSelect style={{ display: 'flex', justifyContent: 'center' }}>
        <WhiteManaButton toggled={cardColors.white} handleClick={() => handleManaButtonClick('white')} />
        <BlueManaButton toggled={cardColors.blue} handleClick={() => handleManaButtonClick('blue')} />
        <BlackManaButton toggled={cardColors.black} handleClick={() => handleManaButtonClick('black')} />
        <RedManaButton toggled={cardColors.red} handleClick={() => handleManaButtonClick('red')} />
        <GreenManaButton toggled={cardColors.green} handleClick={() => handleManaButtonClick('green')} />
        <ColorlessManaButton toggled={cardColors.colorless} handleClick={() => handleManaButtonClick('colorless')} />
      </ColorSelect>
      <ColorTypeSelect>
        <Select value={cardColors.type} onChange={handleColorTypeChange} variant="outlined" style={{ width: '100%' }}>
          <MenuItem value="at-least-these-colors">At Least These Colors</MenuItem>
          <MenuItem value="only-these-colors">Only These Colors</MenuItem>
          <MenuItem value="at-most-these-colors">At Most These Colors</MenuItem>
        </Select>
      </ColorTypeSelect>
    </ColorSelectorWrapper>
  );
};

const ColorSelectorWrapper = styled(Paper)(() => ({ margin: '8px', marginTop: '0px', paddingTop: '8px' }));

const ColorSelect = styled.div(() => ({ display: 'flex', justifyContent: 'center' }));

const ColorTypeSelect = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingTop: '10px',
  paddingBottom: '10px',
}));

export default ColorSelector;
