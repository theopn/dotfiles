from kitty.fast_data_types import Screen
from kitty.tab_bar import (DrawData,
                           TabBarData,
                           ExtraData,
                           draw_tab_with_powerline,
                           )


def draw_tab(
    draw_data: DrawData, screen: Screen, tab: TabBarData,
    before: int, max_title_length: int, index: int, is_last: bool,
    extra_data: ExtraData,
) -> int:
    """
    Kitty's DrawData is defined here:
    https://github.com/kovidgoyal/kitty/blob/83f0d6bc1a9165acc4f938be23cf629a800781e0/kitty/tab_bar.py#L58

    Strat is to edit title_template and active_title_template
    and call the original draw_tab_with_* function.

    See: https://github.com/kovidgoyal/kitty/discussions/4447#discussioncomment-15358832
    """

    layout_icon = "?"
    if tab.layout_name == "tall":
        layout_icon = " "
    elif tab.layout_name == "vertical":
        layout_icon = " "
    elif tab.layout_name == "horizontal":
        layout_icon = " "
    elif tab.layout_name == "stack":
        layout_icon = " "

    # Inject tab index and layout information before the title
    new_draw_data = draw_data._replace(
        title_template="{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}"
        .join(
            [
                "󰘳 {sub.index} ",
                layout_icon,
                "{sup.num_windows}",
                " ",
                " {title}",
            ]
        )

        # active_title_template inherits title_template if nil
    )

    return draw_tab_with_powerline(
        new_draw_data, screen, tab,
        before, max_title_length, index, is_last,
        extra_data)
