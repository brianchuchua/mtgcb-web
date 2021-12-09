import { useDispatch, useSelector } from 'react-redux';
import ColorSelector from '../../../../components/search-form-components/ColorSelector';
import { RootState } from '../../../../redux/rootReducer';
import { ColorTypes, setCardColors, setColorType } from '../../setSlice';

const ConnectedColorSelector: React.FC = () => {
  const dispatch = useDispatch();
  const { cardColors } = useSelector((state: RootState) => state.set);

  const updateCardColors = (color: string) => {
    dispatch(setCardColors(color));
  };

  const updateColorType = (type: ColorTypes) => {
    dispatch(setColorType(type));
  };

  return <ColorSelector cardColors={cardColors} updateCardColors={updateCardColors} updateColorType={updateColorType} />;
};

export default ConnectedColorSelector;
