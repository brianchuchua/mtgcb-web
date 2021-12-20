import { useDispatch, useSelector } from 'react-redux';
import ColorSelector from '../../../../components/search-form-components/ColorSelector';
import { RootState } from '../../../../redux/rootReducer';
import { ColorTypes } from '../../../browse/browseSlice';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedColorSelectorProps extends ConnectedSearchFormComponentProps {
  setColorType: any;
  setCardColors: any;
}

const ConnectedColorSelector: React.FC<ConnectedColorSelectorProps> = ({ reduxSlice, setCardColors, setColorType }) => {
  const dispatch = useDispatch();
  const { cardColors } = useSelector((state: RootState) => state[reduxSlice]);

  const updateCardColors = (color: string) => {
    dispatch(setCardColors(color));
  };

  const updateColorType = (type: ColorTypes) => {
    dispatch(setColorType(type));
  };

  return <ColorSelector cardColors={cardColors} updateCardColors={updateCardColors} updateColorType={updateColorType} />;
};

export default ConnectedColorSelector;
